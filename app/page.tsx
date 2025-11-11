import type { Metadata } from "next";

import HomePageClient from "./_components/HomePageClient";

const appUrl = process.env.NEXT_PUBLIC_APP_URL ?? "http://localhost:3000";
const metadataBase = new URL(appUrl);
const homeTitle = "Cartographerについて | Cartographer";
const homeDescription =
  "Cartographerは、チームの認識ギャップを可視化し、合意形成を後押しするためのワークショップ支援ツールです。";

export const metadata: Metadata = {
  metadataBase,
  title: homeTitle,
  description: homeDescription,
  openGraph: {
    title: homeTitle,
    description: homeDescription,
    url: metadataBase,
    siteName: "Cartographer",
    type: "website",
  },
  twitter: {
    card: "summary_large_image",
    title: homeTitle,
    description: homeDescription,
  },
};

export default function HomePage() {
  return <HomePageClient />;
}
