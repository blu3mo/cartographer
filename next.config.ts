import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // Produce a minimal runtime bundle (Node.js) that we can ship separately
  // from build-time dependencies via Nix two-stage build.
  output: "standalone",
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
