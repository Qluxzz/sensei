import { defineConfig } from "vite"
import elm from "vite-plugin-elm"

export default defineConfig(({ mode }) => ({
  plugins: [
    elm({
      debug: mode !== "production",
      optimize: mode === "production",
    }),
  ],
  build: {
    outDir: "dist/",
    emptyOutDir: true,
  },
}))
