import { defineConfig } from "vite"
import elm from "vite-plugin-elm"
import { VitePWA } from "vite-plugin-pwa"

export default defineConfig(({ mode }) => ({
  plugins: [
    elm({
      debug: mode !== "production",
      optimize: mode === "production",
    }),
    VitePWA({
      registerType: "autoUpdate",
      injectRegister: "inline",
      manifest: {
        short_name: "Elm Template",
        name: "Elm Template",
        description: "This is the description of the Elm Template app",
        icons: [],
        start_url: "/",
        display: "browser",
        background_color: "#ffffff",
        theme_color: "#000000",
      },
      workbox: {
        globPatterns: ["**/*.{js,css,html}"],
      },
    }),
  ],
  build: {
    outDir: "dist/",
    emptyOutDir: true,
  },
}))
