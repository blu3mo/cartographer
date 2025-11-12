"use client";

import type { NavItem } from "@/admin/_components/sidebar-15/data";
import {
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
} from "@/admin/_components/ui/sidebar";

type NavMainProps = {
  items: NavItem[];
};

export function NavMain({ items }: NavMainProps) {
  return (
    <SidebarMenu>
      {items.map((item) => (
        <SidebarMenuItem key={item.title}>
          <SidebarMenuButton asChild isActive={item.isActive}>
            <a href={item.url}>
              <item.icon />
              <span>{item.title}</span>
            </a>
          </SidebarMenuButton>
        </SidebarMenuItem>
      ))}
    </SidebarMenu>
  );
}
