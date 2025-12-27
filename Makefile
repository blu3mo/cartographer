.PHONY: dev supabase-fetch supabase-up supabase-down dev-app db-schema db-reset supabase-init

# 初回セットアップ用: Supabase の docker セットを取得
supabase-fetch:
	infra/supabase/fetch-supabase-compose.sh

# Supabase スタックを起動 (バックグラウンド)
supabase-up:
	@if [ ! -f infra/supabase/bundle/.env ]; then \
		if [ -f infra/supabase/bundle/.env.example ]; then \
			cp infra/supabase/bundle/.env.example infra/supabase/bundle/.env; \
		fi; \
		echo "[supabase-up] .env が無いので infra/supabase/bundle/.env を用意しました（必要なら編集してください）。"; \
	fi
	cd infra/supabase/bundle && docker compose up -d db rest auth kong storage meta

# Supabase スタックを停止
supabase-down:
	cd infra/supabase/bundle && docker compose down

# Supabase のスキーマをローカル DB に適用
db-schema:
	cat supabase/schema.sql | docker compose -f infra/supabase/bundle/docker-compose.yml exec -T db psql -U postgres -d postgres

# ローカル DB をリセットしてマイグレーションを適用
db-reset:
	docker exec supabase-db psql -U postgres -d postgres -c "DROP SCHEMA public CASCADE; CREATE SCHEMA public; GRANT ALL ON SCHEMA public TO postgres;"
	cat supabase/migrations/*.sql | docker exec -i supabase-db psql -U postgres -d postgres

# Supabase の初期セットアップ (何度実行しても安全な想定)
supabase-init: supabase-fetch supabase-up db-schema

# Next.js アプリだけ起動
dev-app:
	npm run dev

# ワンコマンド開発エントリーポイント
# - Supabase を起動
# - Next.js 開発サーバを起動
dev: supabase-up dev-app
