---
title: Using Haskell to generate post template (or, using Haskell in inappropriate places)
layout: post
comments: false
tags: about this blog, haskell
---

我前陣子太閒，就想要自動地在我執行 Github workflow 的時候去觸發一個腳本，從一些網路上的資料庫抓我想要寫的專輯的相關資料。一般來說，做這種事情通常是直接用 Python 或是 Bash script + `jq` 之類的，又簡單又好懂，不過我天生喜歡瞎忙，想說既然都在用 Haskell 了，不如也來用 Haskell 寫這腳本。事實證明真的是比用 Python 麻煩很多...

### 從哪裡抓專輯資料？

我本來以為可以從 Bandcamp 去拉，但我寫信去要 API key 時，得到的回覆是沒有公開 API 可以拿到某張專輯的資訊。網上有一些專案好像可以不用爬蟲不用 API key 去拿到專輯資訊，不過看起來有點醜，就覺得算了。最後決定直接從 [Discogs](https://www.discogs.com/developers) 上面抓。做法是先用 `search` 找到某張專輯的 [master release](https://support.discogs.com/hc/en-us/articles/360005055493-Database-Guidelines-16-Master-Release)，拿到 master release 的 id，再用這個 id 去查找專輯的發行日期、廠牌跟封面。如果這張專輯太冷門，根本沒有 master release 的話，那麼就直接挑第一個 release，用它的 id 去拿我要的資訊。

### 用 Haskell 解析 JSON 資料

處理 JSON 資料時，Haskell 通常用 [aeson](https://hackage.haskell.org/package/aeson)。使用方式是先定義一個資料結構：

```haskell
data Release = Release {
  artists  :: [String],
  title    :: String,
  year     :: Int,
  released :: String,
  imageUrl :: String,
  labels   :: [String],
  uri      :: String
} deriving (Show, Eq, Generic)
```

接著實作 `ToJSON` 和 `FromJSON`。`ToJSON` 很簡單，直接把資料結構轉成 JSON 格式；`FromJSON` 就麻煩多了，得詳細定義如何把 JSON 轉成資料結構：

```haskell
instance ToJSON Release -- 無腦轉

instance FromJSON Release where
  parseJSON (Object v) = do
    artists <- v .: "artists" >>= traverse (.: "name") -- "artists" 是個陣列，這裡的意思是取出陣列中每個元素的 "name"
    title <- v .: "title" -- .: 表示從 v 中取出 "title" 的值
    year <- v .: "year"
    released <- v .: "released"
    images <- v .: "images"
    imageUrl <- case images of
        (img:_) -> img .: "resource_url"
        []      -> fail "No images found"
    labels <- v .: "labels" >>= traverse (.: "name")
    uri <- v .: "uri"
    return Release {
      artists = artists,
      title = title,
      year = year,
      released = released,
      imageUrl = imageUrl,
      labels = labels,
      uri = uri
    }
```

可以看出來，如果資料結構很複雜，解析起來就得定義一堆結構來對應。用 Python 的話，這有點像寫一堆 Pydantic class，但每個 class 都得自己實作 `decode`。

另一個麻煩點是，當你只想取一個深層的值時，Haskell 就有點麻煩了。對比下面兩行程式碼：

```haskell
body ^? key "results" . nth 0 . key (fromString queryKey) . _Integer
```

跟

```python
int(body["results"][0][queryKey])
```

可能我很淺？但我覺得 Python 的好讀多了。另外如果要在 Haskell 做這種操作，需要引入 [lens-aeson](https://hackage.haskell.org/package/lens-aeson) 套件，原生是不支援的。

### 套件管理

Haskell 的套件管理工具有 cabal 和 stack，感覺有點像 pip 和 poetry，但功能重疊得更多。

### 在 Github workflow 裡執行 Haskell 程式

如果每次在 workflow 裡面跑的時候都要重新 `stack build` （可以想像就是 `make build`）的話會花很多時間，實測下來大概需要二十分鐘左右。真的是太久了... 幸好有個專案就是在做 [stack action](https://github.com/freckle/stack-action/tree/v5/)，快取做得蠻好的，所以其實只有第一次提交有改到 Haskell code 的改動時會需要去把整包編好，其他時候就直接用快取的執行檔，那這大概一分鐘之內就能做完。

### 結論

還是用 Python 做這種事情會比較快：

- 比較多人用：Discogs 有提供 Python SDK，甚至不用自己解析 API response。
- Github workflow 設定起來比較不麻煩，而且不會需要花老半天編譯
- JSON support

不過如果你時間很多的話也是個不錯的體驗啦... 所有的程式碼都在 [這裡](https://github.com/usefulalgorithm/usefulalgorithm.github.io/tree/main/scripts/pull_album_info)。
