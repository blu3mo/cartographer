import { redirect } from "next/navigation";

export { generateMetadata } from "./print/page";

type SessionReportPageProps = {
  params: {
    sessionId: string;
    accessToken: string;
    reportId: string;
  };
};

export const dynamic = "force-dynamic";

export default function SessionReportPage({
  params,
}: SessionReportPageProps) {
  redirect(
    `/sessions/${params.sessionId}/${params.accessToken}/reports/${params.reportId}/print`,
  );
}
