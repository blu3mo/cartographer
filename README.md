# Cartographer

Cartographer is a web service that visualizes recognition and facilitates consensus building. It collects and analyzes recognition from multiple people on specific themes (e.g., "current status of this project") using LLM-powered insights.

This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

Cartographer can run with either cloud Supabase or local Supabase via Docker.

### Option 1: Docker Compose (Recommended for Local Development)

Run everything locally with a single command:

```bash
# Copy and configure environment file
cp .env.docker .env
# Edit .env and set your OPENROUTER_API_KEY

# Start all services (Supabase + Web + Agent)
docker compose up
```

The application will be available at:
- Web UI: http://localhost:3000
- Supabase Studio: http://localhost:54321 (username: supabase, password: supabase)

### Option 2: Cloud Supabase

Use a cloud-hosted Supabase project:

```bash
cp .env.example .env.local   # fill in Supabase + OpenRouter keys
npm install
npm run dev                  # Next.js app (http://localhost:3000)
npm run agent                # optional: Ptolemy agent automation
```

Everything after the basics—including schema import, environment tips, and operational flow—is documented in `GUIDE_ja.md`.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
