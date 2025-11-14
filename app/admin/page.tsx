import type { Metadata } from "next";

import { AdminSidebarLayout } from "@/admin/_components/AdminSidebarLayout";

export const metadata: Metadata = {
  title: "Admin Dashboard",
};

export default function AdminPage() {
  return (
    <AdminSidebarLayout />
  );
}
