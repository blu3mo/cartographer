import fs from "node:fs";
import path from "node:path";

import { AgentManager } from "./AgentManager";

function hydrateEnv() {
  const candidates = [".env.agent", ".env.local", ".env"];
  for (const filename of candidates) {
    const fullPath = path.resolve(process.cwd(), filename);
    if (!fs.existsSync(fullPath)) continue;
    const raw = fs.readFileSync(fullPath, "utf-8");
    raw.split(/\r?\n/).forEach((line) => {
      if (!line || line.trim().startsWith("#")) {
        return;
      }
      const index = line.indexOf("=");
      if (index === -1) return;
      const key = line.slice(0, index).trim();
      if (!key || process.env[key]) {
        return;
      }
      const value = line
        .slice(index + 1)
        .trim()
        .replace(/^"|"$/g, "");
      process.env[key] = value;
    });
  }
}

hydrateEnv();

async function main() {
  const manager = new AgentManager();
  await manager.start();

  const shutdown = async () => {
    console.log("\n[AgentManager] Shutting down…");
    await manager.stop();
    process.exit(0);
  };

  process.on("SIGINT", shutdown);
  process.on("SIGTERM", shutdown);
}

void main().catch((error) => {
  console.error("[AgentManager] Failed to start:", error);
  process.exit(1);
});
