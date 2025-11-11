# Cartographer

Cartographer is a web service that visualizes recognition and facilitates consensus building. It collects and analyzes recognition from multiple people on specific themes (e.g., "current status of this project") using LLM-powered insights.

This is a [Next.js](https://nextjs.org) project bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

Cartographer needs a Supabase project and OpenRouter API access. Follow these quick steps, then read `GUIDE_ja.md` for the full setup and troubleshooting guide.

```bash
cp .env.example .env.local   # fill in Supabase + OpenRouter keys
npm install
npm run dev                  # Next.js app (http://localhost:3000)
npm run agent                # optional: Ptolemy agent automation
```

Everything after the basics—including schema import, environment tips, and operational flow—is documented in `GUIDE_ja.md`.

## Stagewise CLI Overlay

Cartographer ships with a `stagewise.json` configuration so you can run the [Stagewise CLI](https://github.com/stagewise-io/stagewise) as a browser toolbar on top of the app.

1. Start the regular dev server (`npm run dev`, default port `3000`).
2. In another terminal (root of this repo), run Stagewise:

   ```bash
   npx stagewise@latest
   # or wrap both in one command
   npx stagewise@latest -- npm run dev
   ```

3. Open `http://localhost:3100` to see the app proxied through Stagewise with the toolbar enabled.

The bundled `stagewise.json` keeps Stagewise on port `3100`, points it to the Next.js dev server (`appPort: 3000`), and leaves plugin auto-detection enabled. Adjust those values if you run the app on different ports or want to pin specific plugins.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
