"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";

import { cn } from "@/lib/utils";

const NAV_LINKS = [
  { href: "/timeline", label: "タイムライン" },
  { href: "/dashboard", label: "ダッシュボード" },
];

export function MarketingNav() {
  const pathname = usePathname();

  return (
    <nav className="flex items-center gap-1">
      {NAV_LINKS.map(({ href, label }) => {
        const isActive =
          pathname === href || (href !== "/" && pathname?.startsWith(`${href}/`));
        return (
          <Link
            key={href}
            href={href}
            className={cn(
              "rounded-full px-3 py-1 text-sm font-semibold transition-colors",
              isActive
                ? "bg-slate-900 text-white"
                : "text-slate-500 hover:bg-slate-100 hover:text-slate-900",
            )}
          >
            {label}
          </Link>
        );
      })}
    </nav>
  );
}
