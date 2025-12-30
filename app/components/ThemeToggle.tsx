"use client";

import { Moon, Sun } from "lucide-react";
import { useEffect, useState } from "react";

export function ThemeToggle() {
  const [theme, setTheme] = useState<"light" | "dark">("light");
  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    setMounted(true);
    // LocalStorageから設定を読み込む
    const savedTheme = localStorage.getItem("theme") as "light" | "dark" | null;
    if (savedTheme) {
      setTheme(savedTheme);
      document.documentElement.classList.toggle("dark", savedTheme === "dark");
    } else {
      // システムのカラーモード設定を確認
      const prefersDark = window.matchMedia(
        "(prefers-color-scheme: dark)",
      ).matches;
      const initialTheme = prefersDark ? "dark" : "light";
      setTheme(initialTheme);
      document.documentElement.classList.toggle("dark", prefersDark);
    }
  }, []);

  const toggleTheme = () => {
    const newTheme = theme === "light" ? "dark" : "light";
    setTheme(newTheme);
    localStorage.setItem("theme", newTheme);
    document.documentElement.classList.toggle("dark", newTheme === "dark");
  };

  // SSR時は何も表示しない（フラッシュ防止）
  if (!mounted) {
    return (
      <div className="h-8 w-16 rounded-full bg-muted border-2 border-border" />
    );
  }

  return (
    <button
      type="button"
      onClick={toggleTheme}
      className={`
        relative inline-flex h-8 w-16 items-center rounded-full transition-all duration-300
        border-2 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2
        ${theme === "dark" ? "bg-slate-700 border-slate-600" : "bg-slate-200 border-slate-300"}
        hover:opacity-80 active:scale-95
      `}
      aria-label={`${theme === "light" ? "ダーク" : "ライト"}モードに切り替え`}
      role="switch"
      aria-checked={theme === "dark"}
    >
      <span
        className={`
          inline-flex h-6 w-6 transform items-center justify-center rounded-full
          bg-white shadow-md transition-transform duration-300
          ${theme === "dark" ? "translate-x-8" : "translate-x-0.5"}
        `}
      >
        {theme === "light" ? (
          <Sun className="h-4 w-4 text-amber-500" />
        ) : (
          <Moon className="h-4 w-4 text-slate-700" />
        )}
      </span>
    </button>
  );
}
