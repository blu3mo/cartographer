import type { Metadata } from "next";

const SITE_NAME = "倍速会議";
const DEFAULT_DESCRIPTION = "チームの認識を可視化し、合意形成を促進するツール";
const DEFAULT_SITE_URL =
  process.env.NEXT_PUBLIC_SITE_URL ?? "http://localhost:3000";
const DEFAULT_OG_IMAGE = "/og-default.png";

const sharedOpenGraph = {
  siteName: SITE_NAME,
  locale: "ja_JP",
  type: "website",
  images: [
    {
      url: DEFAULT_OG_IMAGE,
      width: 1200,
      height: 630,
      alt: `${SITE_NAME} | ${DEFAULT_DESCRIPTION}`,
    },
  ],
} satisfies NonNullable<Metadata["openGraph"]>;

const sharedTwitter = {
  card: "summary_large_image",
  images: [DEFAULT_OG_IMAGE],
} satisfies NonNullable<Metadata["twitter"]>;

export function buildMetadata(opts: {
  title?: string;
  description?: string;
  url?: string;
}): Metadata {
  const title =
    opts.title ?? `${SITE_NAME} - 認識を可視化し、合意形成を促進する`;
  const description = opts.description ?? DEFAULT_DESCRIPTION;

  return {
    metadataBase: new URL(DEFAULT_SITE_URL),
    title,
    description,
    openGraph: {
      ...sharedOpenGraph,
      title,
      description,
      url: opts.url,
    },
    twitter: {
      ...sharedTwitter,
      title,
      description,
    },
  };
}

export function truncateForMeta(text: string, maxLength = 120): string {
  if (text.length <= maxLength) return text;
  return `${text.slice(0, maxLength)}…`;
}

export { DEFAULT_DESCRIPTION, DEFAULT_OG_IMAGE, SITE_NAME };
