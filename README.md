# Cartographer

Cartographer is a web service that visualizes recognition and facilitates consensus building. It collects and analyzes recognition from multiple people on specific themes (e.g., "current status of this project") using LLM-powered insights.

This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

Cartographer can be run in two ways:

### Option 1: Cloud Services (Supabase + OpenRouter)

Cartographer needs a Supabase project and OpenRouter API access. Follow these quick steps, then read `GUIDE_ja.md` for the full setup and troubleshooting guide.

```bash
cp .env.example .env.local   # fill in Supabase + OpenRouter keys
npm install
npm run dev                  # Next.js app (http://localhost:3000)
npm run agent                # optional: Ptolemy agent automation
```

Everything after the basics—including schema import, environment tips, and operational flow—is documented in `GUIDE_ja.md`.

### Option 2: Docker Compose (Self-Hosted)

Run Cartographer completely locally without external dependencies using Docker Compose. This includes a local PostgreSQL database with Supabase stack and supports both OpenRouter and Google Vertex AI for LLM.

```bash
cp .env.docker.example .env.docker   # configure your settings
docker-compose --env-file .env.docker up -d
```

Access the application at http://localhost:3000. See `DOCKER_SETUP.md` for detailed instructions.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
