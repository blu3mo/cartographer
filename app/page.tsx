import type { Metadata } from "next";
import { redirect } from "next/navigation";

const appUrl = process.env.NEXT_PUBLIC_APP_URL ?? "http://localhost:3000";
const metadataBase = new URL(appUrl);
const homeTitle = "Cartographer | チームの認識をマップする";
const homeDescription =
  "Cartographerのセッションダッシュボードで、議論の進行状況と参加状況をまとめて管理できます。";

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
  redirect("/dashboard");
}
