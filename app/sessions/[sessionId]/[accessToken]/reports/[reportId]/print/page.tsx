import type { Metadata } from "next";

import { requireSessionAdminToken } from "@/lib/server/session-access";
import SessionReportPrintClient from "./ReportPrintClient";

const appUrl = process.env.NEXT_PUBLIC_APP_URL ?? "http://localhost:3000";
const metadataBase = new URL(appUrl);

export const dynamic = "force-dynamic";

type PageParams = {
  sessionId: string;
  accessToken: string;
  reportId: string;
};

export async function generateMetadata({
  params,
}: {
  params: PageParams;
}): Promise<Metadata> {
  const fallbackTitle = "セッションレポート | Cartographer";
  const fallbackDescription =
    "管理者が共有したCartographerレポートの印刷ビューです。";

  try {
    const session = await requireSessionAdminToken(
      params.sessionId,
      params.accessToken,
    );
    const baseTitle = session.title?.trim() || "名称未設定のセッション";
    const title = `${baseTitle} | Cartographer レポート`;
    const background = session.context?.trim();
    const goal = session.goal?.trim();
    const descriptionSegments = [
      background ? `背景: ${background}` : null,
      goal ? `目的: ${goal}` : null,
    ].filter(Boolean) as string[];
    const description = descriptionSegments.join(" / ") || fallbackDescription;

    const pageUrl = new URL(
      `/sessions/${params.sessionId}/${params.accessToken}/reports/${params.reportId}/print`,
      metadataBase,
    );

    return {
      metadataBase,
      title,
      description,
      openGraph: {
        title,
        description,
        type: "article",
        url: pageUrl,
        siteName: "Cartographer",
      },
      twitter: {
        card: "summary_large_image",
        title,
        description,
      },
    };
  } catch (error) {
    console.error("Failed to build report print metadata", error);
    return {
      metadataBase,
      title: fallbackTitle,
      description: fallbackDescription,
    };
  }
}

export default function SessionReportPrintPage({
  params,
}: {
  params: PageParams;
}) {
  return <SessionReportPrintClient {...params} />;
}
