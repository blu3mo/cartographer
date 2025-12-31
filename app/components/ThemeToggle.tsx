"use client";

import { Moon, Sun } from "lucide-react";
import { useTheme } from "next-themes";
import { useEffect, useState } from "react";
import { Switch } from "@/components/ui/switch";
import { Label } from "@/components/ui/label";

export function ThemeToggle() {
  const { theme, setTheme } = useTheme();
  const [mounted, setMounted] = useState(false);

  useEffect(() => {
    setMounted(true);
  }, []);

  const toggleTheme = () => {
    setTheme(theme === "light" ? "dark" : "light");
  };

  // SSR時は何も表示しない（フラッシュ防止）
  if (!mounted) {
    return (
      <div className="flex items-center gap-2 opacity-50">
        <Sun className="h-4 w-4" />
        <Switch disabled />
        <Moon className="h-4 w-4" />
      </div>
    );
  }

  const isDark = theme === "dark";

  return (
    <div className="flex items-center gap-2">
      <Label htmlFor="theme-toggle" className="sr-only">
        テーマ切り替え
      </Label>
      <Sun
        className={`h-4 w-4 transition-colors ${
          isDark ? "text-muted-foreground" : "text-amber-500"
        }`}
        aria-hidden="true"
      />
      <Switch
        id="theme-toggle"
        checked={isDark}
        onCheckedChange={toggleTheme}
        aria-label={`${isDark ? "ライト" : "ダーク"}モードに切り替え`}
      />
      <Moon
        className={`h-4 w-4 transition-colors ${
          isDark ? "text-primary" : "text-muted-foreground"
        }`}
        aria-hidden="true"
      />
    </div>
  );
}
