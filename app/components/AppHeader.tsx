"use client";

import Link from "next/link";

import { cn } from "@/lib/utils";

interface AppHeaderProps {
  className?: string;
}

export function AppHeader({ className }: AppHeaderProps) {
  return (
    <header className="sticky top-0 z-50 w-full border-b border-slate-200 bg-white">
      <div
        className={cn(
          "mx-auto flex items-center px-4 py-4 sm:px-6 lg:px-8",
          className,
        )}
      >
        <Link
          href="/"
          className="text-3xl font-bold tracking-tight text-slate-900 transition-colors hover:text-slate-700 sm:text-4xl"
        >
          Cartographer
        </Link>
      </div>
    </header>
  );
}
