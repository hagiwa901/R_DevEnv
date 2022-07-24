# 概要
Rの開発環境

# 環境
- docker

# 実行方法
## dockerコンテナの立ち上げ
```
docker-compose up run_program
```
## dockerコンテナが立ち上がっているか確認
```
docker-compose ps
```
## dockerコンテナに入る
```
docker-compose exec run_program [psコマンド]
```
## dockerコンテナを下ろす
```
docker-compose down -v
```
