"use client";

import { cva, type VariantProps } from "class-variance-authority";
import { Loader2 } from "lucide-react";
import * as React from "react";

import { cn } from "@/lib/utils";

const buttonVariants = cva(
  "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-semibold transition-all focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-900 dark:focus-visible:ring-slate-100 focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg]:size-4 [&_svg]:shrink-0",
  {
    variants: {
      variant: {
        default:
          "shadow hover:shadow-md active:scale-[0.98]",
        destructive:
          "bg-red-600 text-white shadow hover:bg-red-700 hover:shadow-md active:scale-[0.98] dark:bg-red-500 dark:text-white dark:hover:bg-red-600",
        outline:
          "border-2 border-slate-300 bg-white text-slate-900 hover:bg-slate-50 hover:border-slate-400 active:scale-[0.98] dark:border-slate-600 dark:bg-slate-900 dark:text-white dark:hover:bg-slate-800 dark:hover:border-slate-500",
        secondary:
          "bg-slate-200 text-slate-900 border border-slate-300 shadow-sm hover:bg-slate-300 hover:shadow active:scale-[0.98] dark:bg-slate-800 dark:text-slate-100 dark:border-slate-700 dark:hover:bg-slate-700",
        ghost: "hover:bg-slate-100 hover:text-slate-900 dark:hover:bg-slate-800 dark:hover:text-slate-100",
        link: "text-slate-900 underline-offset-4 hover:underline dark:text-slate-100",
      },
      size: {
        default: "h-10 px-4 py-2",
        sm: "h-9 rounded-md px-3 text-xs",
        lg: "h-11 rounded-md px-8",
        icon: "h-10 w-10",
      },
    },
    defaultVariants: {
      variant: "default",
      size: "default",
    },
  },
);

export interface ButtonProps
  extends React.ButtonHTMLAttributes<HTMLButtonElement>,
    VariantProps<typeof buttonVariants> {
  isLoading?: boolean;
}

const Button = React.forwardRef<HTMLButtonElement, ButtonProps>(
  (
    { className, variant, size, isLoading, children, disabled, ...props },
    ref,
  ) => {
    const [isDark, setIsDark] = React.useState(false);

    React.useEffect(() => {
      const checkDarkMode = () => {
        setIsDark(document.documentElement.classList.contains("dark"));
      };

      checkDarkMode();

      // MutationObserverでdarkクラスの変更を監視
      const observer = new MutationObserver(checkDarkMode);
      observer.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ["class"],
      });

      return () => observer.disconnect();
    }, []);

    // デフォルトボタンのスタイルを動的に設定
    const defaultButtonStyle = (variant === "default" || !variant) ? {
      backgroundColor: isDark ? '#ffffff' : '#0f172a',
      color: isDark ? '#0f172a' : '#ffffff',
    } : {};

    return (
      <button
        className={cn(buttonVariants({ variant, size, className }))}
        style={defaultButtonStyle}
        ref={ref}
        disabled={disabled || isLoading}
        {...props}
      >
        {isLoading && <Loader2 className="h-4 w-4 animate-spin" />}
        {children}
      </button>
    );
  },
);
Button.displayName = "Button";

export { Button, buttonVariants };
