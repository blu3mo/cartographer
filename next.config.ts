import type { NextConfig } from "next";
import path from "path";
import { fileURLToPath } from "url";

const projectRoot = path.dirname(fileURLToPath(import.meta.url));

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
  turbopack: {
    root: projectRoot,
  },
};

export default nextConfig;
