import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // Produce a minimal runtime bundle (Node.js) that we can ship separately
  // from build-time dependencies via Nix two-stage build.
  output: "standalone",
  // Ensure tracing stays inside this repo and copies only .env.local (not .env)
  outputFileTracingRoot: __dirname,
  outputFileTracingExcludes: {
    "*": ["**/.env"],
  },
  outputFileTracingIncludes: {
    "*": [".env.local"],
  },
  images: {
    remotePatterns: [
      {
        protocol: "https",
        hostname: "api.qrserver.com",
      },
    ],
  },
};

export default nextConfig;
