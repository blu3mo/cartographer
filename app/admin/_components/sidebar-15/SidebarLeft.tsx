"use client";

import { adminSidebarData } from "@/admin/_components/sidebar-15/data";
import { NavFavorites } from "@/admin/_components/sidebar-15/NavFavorites";
import { NavMain } from "@/admin/_components/sidebar-15/NavMain";
import { NavSecondary } from "@/admin/_components/sidebar-15/NavSecondary";
import { NavWorkspaces } from "@/admin/_components/sidebar-15/NavWorkspaces";
import { TeamSwitcher } from "@/admin/_components/sidebar-15/TeamSwitcher";
import {
  Sidebar,
  SidebarContent,
  SidebarHeader,
  SidebarRail,
} from "@/admin/_components/ui/sidebar";

export function SidebarLeft({
  ...props
}: React.ComponentProps<typeof Sidebar>) {
  return (
    <Sidebar className="border-r-0" {...props}>
      <SidebarHeader>
        <TeamSwitcher teams={adminSidebarData.teams} />
        <NavMain items={adminSidebarData.navMain} />
      </SidebarHeader>
      <SidebarContent>
        <NavFavorites favorites={adminSidebarData.favorites} />
        <NavWorkspaces workspaces={adminSidebarData.workspaces} />
        <NavSecondary
          items={adminSidebarData.navSecondary}
          className="mt-auto"
        />
      </SidebarContent>
      <SidebarRail />
    </Sidebar>
  );
}
