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
        border: "var(--border)",
        input: "var(--input)",
        ring: "var(--ring)",
        background: "var(--background)",
        foreground: "var(--foreground)",
        primary: {
          DEFAULT: "var(--primary)",
          foreground: "var(--primary-foreground)",
        },
        secondary: {
          DEFAULT: "var(--secondary)",
          foreground: "var(--secondary-foreground)",
        },
        destructive: {
          DEFAULT: "var(--destructive)",
          foreground: "var(--destructive-foreground)",
        },
        muted: {
          DEFAULT: "var(--muted)",
          foreground: "var(--muted-foreground)",
        },
        accent: {
          DEFAULT: "var(--accent)",
          foreground: "var(--accent-foreground)",
        },
        popover: {
          DEFAULT: "var(--popover)",
          foreground: "var(--popover-foreground)",
        },
        card: {
          DEFAULT: "var(--card)",
          foreground: "var(--card-foreground)",
        },
        status: {
          pending: "var(--status-pending)",
          "pending-foreground": "var(--status-pending-foreground)",
          generating: "var(--status-generating)",
          "generating-foreground": "var(--status-generating-foreground)",
          completed: "var(--status-completed)",
          "completed-foreground": "var(--status-completed-foreground)",
          failed: "var(--status-failed)",
          "failed-foreground": "var(--status-failed-foreground)",
        },
        sentiment: {
          neutral: "var(--sentiment-neutral)",
          "neutral-foreground": "var(--sentiment-neutral-foreground)",
          positive: "var(--sentiment-positive)",
          "positive-foreground": "var(--sentiment-positive-foreground)",
          "strong-positive": "var(--sentiment-strong-positive)",
          "strong-positive-foreground":
            "var(--sentiment-strong-positive-foreground)",
          negative: "var(--sentiment-negative)",
          "negative-foreground": "var(--sentiment-negative-foreground)",
        },
        neutral: {
          DEFAULT: "var(--neutral)",
          foreground: "var(--neutral-foreground)",
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
