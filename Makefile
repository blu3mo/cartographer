.PHONY: dev supabase-fetch supabase-up supabase-down supabase-migrate supabase-reset supabase-remove backend-up dev-app

# Supabase docker setup を取得
supabase-fetch:
	infra/supabase/fetch-supabase-compose.sh

supabase-remove:
	rm -rf infra/supabase/bundle

supabase-up:
	@if [ ! -f infra/supabase/bundle/.env ]; then \
		if [ -f infra/supabase/bundle/.env.example ]; then \
			cp infra/supabase/bundle/.env.example infra/supabase/bundle/.env; \
		fi; \
		echo "[supabase-up] .env が無いので infra/supabase/bundle/.env を用意しました（必要なら編集してください）。"; \
	fi
	cd infra/supabase/bundle && docker compose up -d db rest auth kong storage meta

supabase-down:
	cd infra/supabase/bundle && docker compose down

supabase-migrate: supabase-up
	cat supabase/migrations/*.sql | docker exec -i supabase-db psql -U postgres -d postgres

supabase-reset: supabase-remove supabase-fetch supabase-up supabase-migrate

# Haskell バックエンドを起動 (nix run)
backend-up:
	DATABASE_URL=postgresql://postgres:your-super-secret-and-long-postgres-password@localhost:54322/postgres nix run .#cartographer-backend

# Next.js 開発サーバ
dev-app:
	npm run dev

# ワンコマンド開発エントリーポイント (process-compose で並列起動)
dev:
	nix run nixpkgs#process-compose -- up --port 8081
