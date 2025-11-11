import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "Cartographer - 認識を可視化し、合意形成を促進する",
  description:
    "Cartographerとは、「それぞれの認識を洗い出し、合意点、相違点、不明点を可視化するツール」です。",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body className="antialiased">{children}</body>
    </html>
  );
}
