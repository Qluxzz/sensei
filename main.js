import { Elm } from "./src/Main.elm"

const key = "@sensei-statistics"

const statistics = (() => {
  try {
    // User might have disabled access to local storage
    const existing = localStorage.getItem(key)
    if (!existing) return null

    return JSON.parse(existing)
  } catch {
    return null
  }
})()

const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: statistics,
})

app.ports.persistStatistics.subscribe((statistics) => {
  try {
    localStorage.setItem(key, JSON.stringify(statistics))
  } catch {
    // If the user has disabled access to local storage
    // there really isn't a lot to do
  }
})
