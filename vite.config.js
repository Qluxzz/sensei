import { defineConfig } from "vite"
import elm from "vite-plugin-elm"
import { VitePWA } from "vite-plugin-pwa"

export default defineConfig(({ mode }) => ({
  base: "/sensei/",
  plugins: [
    elm({
      debug: mode !== "production",
      optimize: mode === "production",
    }),
    VitePWA({
      registerType: "autoUpdate",
      injectRegister: "inline",
      manifest: {
        short_name: "Sensei",
        name: "Sensei",
        description:
          "Sensei is an online app made in order to make you learn japanese words and their usages",
        icons: [],
        start_url: "/sensei",
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
