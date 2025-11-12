import type { Metadata } from "next";

import { AdminSidebarLayout } from "@/admin/_components/AdminSidebarLayout";

export const metadata: Metadata = {
  title: "Admin Dashboard",
};

export default function AdminPage() {
  return (
    <div className="min-h-screen bg-background">
      <AdminSidebarLayout />
    </div>
  );
}
