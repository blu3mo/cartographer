# Cartographer

Cartographer is a web service that visualizes recognition and facilitates consensus building. It collects and analyzes recognition from multiple people on specific themes (e.g., "current status of this project") using LLM-powered insights.

This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

### Prerequisites

**⚠️ Important:** This application requires a real PostgreSQL database and OpenRouter API access. You cannot run it without these services.

Before running the development server, you need to set up environment variables.

1. **Copy the example environment file**:

```bash
cp env.example .env.local
```

2. **Set up the database**:

   - Create a PostgreSQL database using [Neon](https://neon.tech) or [Supabase](https://supabase.com)
   - Get your connection string from the database dashboard
   - Replace the `DATABASE_URL` in `.env.local` with your connection string

3. **Set up OpenRouter API**:

   - Sign up at [OpenRouter](https://openrouter.ai)
   - Get your API key from the [keys page](https://openrouter.ai/keys)
   - Replace `OPENROUTER_API_KEY` in `.env.local` with your API key

4. **Run database migrations**:

```bash
npx prisma migrate dev
```

### Run the development server

```bash
npm run dev
# or
yarn dev
# or
pnpm dev
# or
bun dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

You can start editing the page by modifying `app/page.tsx`. The page auto-updates as you edit the file.

This project uses [`next/font`](https://nextjs.org/docs/app/building-your-application/optimizing/fonts) to automatically optimize and load [Geist](https://vercel.com/font), a new font family for Vercel.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
