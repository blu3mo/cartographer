import type { Config } from "tailwindcss";

const config: Config = {
  darkMode: "class",
  content: [
    "./app/**/*.{js,ts,jsx,tsx,mdx}",
    "./components/**/*.{js,ts,jsx,tsx,mdx}",
  ],
  theme: {
    extend: {
      colors: {
        border: "hsl(var(--border))",
        input: "hsl(var(--input))",
        ring: "hsl(var(--ring))",
        background: "hsl(var(--background))",
        foreground: "hsl(var(--foreground))",
        primary: {
          DEFAULT: "hsl(var(--primary))",
          foreground: "hsl(var(--primary-foreground))",
        },
        secondary: {
          DEFAULT: "hsl(var(--secondary))",
          foreground: "hsl(var(--secondary-foreground))",
        },
        destructive: {
          DEFAULT: "hsl(var(--destructive))",
          foreground: "hsl(var(--destructive-foreground))",
        },
        muted: {
          DEFAULT: "hsl(var(--muted))",
          foreground: "hsl(var(--muted-foreground))",
        },
        accent: {
          DEFAULT: "hsl(var(--accent))",
          foreground: "hsl(var(--accent-foreground))",
        },
        popover: {
          DEFAULT: "hsl(var(--popover))",
          foreground: "hsl(var(--popover-foreground))",
        },
        card: {
          DEFAULT: "hsl(var(--card))",
          foreground: "hsl(var(--card-foreground))",
        },
        status: {
          pending: "hsl(var(--status-pending))",
          "pending-foreground": "hsl(var(--status-pending-foreground))",
          generating: "hsl(var(--status-generating))",
          "generating-foreground": "hsl(var(--status-generating-foreground))",
          completed: "hsl(var(--status-completed))",
          "completed-foreground": "hsl(var(--status-completed-foreground))",
          failed: "hsl(var(--status-failed))",
          "failed-foreground": "hsl(var(--status-failed-foreground))",
        },
        sentiment: {
          neutral: "hsl(var(--sentiment-neutral))",
          "neutral-foreground": "hsl(var(--sentiment-neutral-foreground))",
          positive: "hsl(var(--sentiment-positive))",
          "positive-foreground": "hsl(var(--sentiment-positive-foreground))",
          "strong-positive": "hsl(var(--sentiment-strong-positive))",
          "strong-positive-foreground": "hsl(var(--sentiment-strong-positive-foreground))",
          negative: "hsl(var(--sentiment-negative))",
          "negative-foreground": "hsl(var(--sentiment-negative-foreground))",
        },
        neutral: {
          DEFAULT: "hsl(var(--neutral))",
          foreground: "hsl(var(--neutral-foreground))",
        },
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
      fontFamily: {
        sans: [
          "var(--font-geist-sans)",
          "ui-sans-serif",
          "system-ui",
          "sans-serif",
        ],
        mono: ["var(--font-geist-mono)", "ui-monospace", "monospace"],
      },
    },
  },
  plugins: [require("@tailwindcss/typography")],
};

export default config;
