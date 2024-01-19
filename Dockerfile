# ベースイメージの指定
FROM haskell:8.8

# 作業ディレクトリの設定
WORKDIR /app

# アプリケーションのソースコードと依存関係ファイルのコピー
COPY . /app

# 依存関係のインストール
RUN stack setup
RUN stack build --dependencies-only

# アプリケーションのビルド
RUN stack build

# コンテナ起動時のコマンド指定
CMD ["stack", "exec", "spock20240105-exe"]
