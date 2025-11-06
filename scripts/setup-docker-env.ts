import { randomBytes, createHmac } from "node:crypto";
import { existsSync, writeFileSync } from "node:fs";
import path from "node:path";

type CliOptions = {
  output: string;
  force: boolean;
};

function parseArgs(): CliOptions {
  const args = process.argv.slice(2);
  let output = ".env.docker.local";
  let force = false;

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === "--force") {
      force = true;
    } else if ((arg === "--output" || arg === "-o") && args[i + 1]) {
      output = args[i + 1];
      i++;
    } else {
      console.warn(`Unknown argument: ${arg}`);
    }
  }

  return { output, force };
}

function randomString(bytes: number) {
  return randomBytes(bytes).toString("base64");
}

function base64UrlEncode(obj: Record<string, unknown>) {
  return Buffer.from(JSON.stringify(obj)).toString("base64url");
}

function createJwt(payload: Record<string, unknown>, secret: string) {
  const header = { alg: "HS256", typ: "JWT" };
  const body = `${base64UrlEncode(header)}.${base64UrlEncode(payload)}`;
  const signature = createHmac("sha256", secret)
    .update(body)
    .digest("base64url");
  return `${body}.${signature}`;
}

function main() {
  const { output, force } = parseArgs();
  const targetPath = path.resolve(output);

  if (existsSync(targetPath) && !force) {
    console.error(
      `Error: ${output} already exists. Re-run with --force to overwrite.`,
    );
    process.exitCode = 1;
    return;
  }

  const jwtSecret = randomString(48);
  const exp = Math.floor(Date.now() / 1000) + 60 * 60 * 24 * 365 * 10;
  const anonKey = createJwt(
    { iss: "supabase-local", role: "anon", exp },
    jwtSecret,
  );
  const serviceRoleKey = createJwt(
    { iss: "supabase-local", role: "service_role", exp },
    jwtSecret,
  );

  const template = `############
# Secrets (auto-generated)
############

POSTGRES_PASSWORD=${randomString(32)}
JWT_SECRET=${jwtSecret}
ANON_KEY=${anonKey}
SERVICE_ROLE_KEY=${serviceRoleKey}
DASHBOARD_USERNAME=supabase
DASHBOARD_PASSWORD=supabase
SECRET_KEY_BASE=${randomString(48)}
PG_META_CRYPTO_KEY=${randomString(32)}

############
# Database
############

POSTGRES_HOST=db
POSTGRES_DB=postgres
POSTGRES_PORT=5432

############
# API Proxy - Kong
############

KONG_HTTP_PORT=54321
KONG_HTTPS_PORT=54322

############
# API - PostgREST
############

PGRST_DB_SCHEMAS=public,storage
JWT_EXPIRY=3600

############
# Logs - Analytics
############

LOGFLARE_PUBLIC_ACCESS_TOKEN=${randomString(24)}
LOGFLARE_PRIVATE_ACCESS_TOKEN=${randomString(24)}

############
# Cartographer - Web & Agent
############

# Public URL for browser access (localhost for local development)
NEXT_PUBLIC_SUPABASE_URL=http://localhost:54321

# OpenRouter API Key for LLM
OPENROUTER_API_KEY=

# Web server port
WEB_PORT=3000

# Node environment
NODE_ENV=development
`;

  writeFileSync(targetPath, template, { encoding: "utf8", mode: 0o600 });
  console.log(`Wrote secrets to ${output}`);
}

main();
