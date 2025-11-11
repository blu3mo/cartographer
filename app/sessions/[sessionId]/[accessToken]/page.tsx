import { redirect } from "next/navigation";

type SessionAdminRedirectPageProps = {
  params: {
    sessionId: string;
    accessToken: string;
  };
};

export const dynamic = "force-dynamic";

export default function SessionAdminRedirectPage({
  params,
}: SessionAdminRedirectPageProps) {
  redirect(`/?sessionId=${params.sessionId}`);
}
