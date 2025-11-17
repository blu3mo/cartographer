"use client";

import Link from "next/link";

import { cn } from "@/lib/utils";

interface AppHeaderProps {
  className?: string;
  rightSlot?: React.ReactNode;
  children?: React.ReactNode;
}

export function AppHeader({ className, rightSlot, children }: AppHeaderProps) {
  return (
    <header className="sticky top-0 z-50 w-full border-b border-slate-200 bg-white">
      <div
        className={cn(
          "mx-auto flex items-center gap-4 px-6 py-5 sm:px-8 lg:px-10",
          className,
        )}
      >
        <Link
          href="/"
          className="flex items-end gap-3 text-2xl font-bold tracking-tight text-slate-900 transition-colors hover:text-slate-700 sm:text-3xl"
        >
          <span className="leading-none">Cartographer</span>
          <span className="text-[10px] font-semibold uppercase tracking-[0.3em] text-slate-600 leading-none pb-0.5 sm:text-xs">
            ベータ版
          </span>
        </Link>
        {(children || rightSlot) && (
          <div className="flex flex-1 items-center gap-4">
            {children && (
              <div className="flex flex-1 items-center gap-2">{children}</div>
            )}
            {rightSlot && (
              <div className="ml-auto flex items-center gap-3">{rightSlot}</div>
            )}
          </div>
        )}
      </div>
    </header>
  );
}
