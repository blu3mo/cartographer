import axios from "axios";

export interface LLMMessage {
  role: "system" | "user" | "assistant";
  content: string;
}

export interface LLMProvider {
  callLLM(messages: LLMMessage[]): Promise<string>;
}

class OpenRouterProvider implements LLMProvider {
  private readonly apiUrl = "https://openrouter.ai/api/v1/chat/completions";
  private readonly model = "google/gemini-2.5-pro";
  private readonly apiKey: string;

  constructor(apiKey: string) {
    this.apiKey = apiKey;
  }

  async callLLM(messages: LLMMessage[]): Promise<string> {
    console.log("=== LLM Input (OpenRouter) ===");
    console.log("Model:", this.model);
    messages.forEach((msg, index) => {
      console.log(`\n[Message ${index + 1}]`);
      console.log(`Role: ${msg.role}`);
      console.log(msg.content);
    });
    console.log("\n================\n");

    try {
      const response = await axios.post(
        this.apiUrl,
        {
          model: this.model,
          messages,
        },
        {
          headers: {
            Authorization: `Bearer ${this.apiKey}`,
            "Content-Type": "application/json",
            "HTTP-Referer": "https://cartographer.app",
            "X-Title": "Cartographer",
          },
          timeout: 30000,
        },
      );

      const output = response.data.choices[0].message.content;

      console.log("=== LLM Output ===");
      console.log(output);
      console.log("==================\n");

      return output;
    } catch (error) {
      if (axios.isAxiosError(error)) {
        console.error("LLM API Error:", {
          status: error.response?.status,
          statusText: error.response?.statusText,
          data: error.response?.data,
        });

        if (error.response?.status === 429) {
          throw new Error("Rate limit exceeded. Please try again in a moment.");
        }
      }
      console.error("LLM API Error:", error);
      throw new Error("Failed to call LLM API");
    }
  }
}

class VertexAIProvider implements LLMProvider {
  private readonly model = "gemini-2.5-pro";
  private readonly projectId: string;
  private readonly location = "us-central1";
  private accessToken: string | null = null;
  private tokenExpiry: number = 0;

  constructor(projectId: string) {
    this.projectId = projectId;
  }

  private async getAccessToken(): Promise<string> {
    if (this.accessToken && Date.now() < this.tokenExpiry) {
      return this.accessToken;
    }

    try {
      const { GoogleAuth } = await import("google-auth-library");
      const auth = new GoogleAuth({
        scopes: ["https://www.googleapis.com/auth/cloud-platform"],
      });

      const client = await auth.getClient();
      const tokenResponse = await client.getAccessToken();

      if (!tokenResponse.token) {
        throw new Error("Failed to get access token");
      }

      this.accessToken = tokenResponse.token;
      this.tokenExpiry = Date.now() + 50 * 60 * 1000;

      return this.accessToken;
    } catch (error) {
      console.error("Failed to get Vertex AI access token:", error);
      throw new Error("Failed to authenticate with Vertex AI");
    }
  }

  async callLLM(messages: LLMMessage[]): Promise<string> {
    console.log("=== LLM Input (Vertex AI) ===");
    console.log("Model:", this.model);
    console.log("Project:", this.projectId);
    messages.forEach((msg, index) => {
      console.log(`\n[Message ${index + 1}]`);
      console.log(`Role: ${msg.role}`);
      console.log(msg.content);
    });
    console.log("\n================\n");

    try {
      const accessToken = await this.getAccessToken();

      const contents = messages.map((msg) => ({
        role: msg.role === "assistant" ? "model" : "user",
        parts: [{ text: msg.content }],
      }));

      const mergedContents = [];
      for (const content of contents) {
        if (
          mergedContents.length > 0 &&
          mergedContents[mergedContents.length - 1].role === content.role
        ) {
          mergedContents[mergedContents.length - 1].parts.push(
            ...content.parts,
          );
        } else {
          mergedContents.push(content);
        }
      }

      const endpoint = `https://${this.location}-aiplatform.googleapis.com/v1/projects/${this.projectId}/locations/${this.location}/publishers/google/models/${this.model}:generateContent`;

      const response = await axios.post(
        endpoint,
        {
          contents: mergedContents,
          generationConfig: {
            temperature: 0.7,
            maxOutputTokens: 8192,
          },
        },
        {
          headers: {
            Authorization: `Bearer ${accessToken}`,
            "Content-Type": "application/json",
          },
          timeout: 30000,
        },
      );

      const output =
        response.data.candidates?.[0]?.content?.parts?.[0]?.text || "";

      console.log("=== LLM Output ===");
      console.log(output);
      console.log("==================\n");

      return output;
    } catch (error) {
      if (axios.isAxiosError(error)) {
        console.error("Vertex AI API Error:", {
          status: error.response?.status,
          statusText: error.response?.statusText,
          data: error.response?.data,
        });
      }
      console.error("Vertex AI API Error:", error);
      throw new Error("Failed to call Vertex AI API");
    }
  }
}

export function createLLMProvider(): LLMProvider {
  const useVertexAI = process.env.USE_VERTEX_AI === "true";

  if (useVertexAI) {
    const projectId = process.env.GOOGLE_CLOUD_PROJECT;
    if (!projectId) {
      throw new Error(
        "GOOGLE_CLOUD_PROJECT is required when USE_VERTEX_AI is true",
      );
    }
    console.log("Using Vertex AI provider");
    return new VertexAIProvider(projectId);
  }

  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error("OPENROUTER_API_KEY is required when not using Vertex AI");
  }
  console.log("Using OpenRouter provider");
  return new OpenRouterProvider(apiKey);
}
