"use client";

import type { ReactNode } from "react";

import Link from "next/link";

import { cn } from "@/lib/utils";

interface AppHeaderProps {
  className?: string;
  rightSlot?: ReactNode;
}

export function AppHeader({ className, rightSlot }: AppHeaderProps) {
  return (
    <header className="sticky top-0 z-50 w-full border-b border-slate-200 bg-white">
      <div
        className={cn(
          "mx-auto flex items-center justify-between gap-4 px-4 py-4 sm:px-6 lg:px-8",
          className,
        )}
      >
        <Link
          href="/"
          className="text-3xl font-bold tracking-tight text-slate-900 transition-colors hover:text-slate-700 sm:text-4xl"
        >
          Cartographer
        </Link>
        {rightSlot ? (
          <div className="flex items-center gap-2">{rightSlot}</div>
        ) : null}
      </div>
    </header>
  );
}
