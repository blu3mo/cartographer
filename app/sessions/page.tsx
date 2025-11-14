import { permanentRedirect } from "next/navigation";

export default function SessionsIndexPage() {
  permanentRedirect("/timeline");
}
