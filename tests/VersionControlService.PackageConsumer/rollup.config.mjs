// This config bundles the oldest-Fable consumer and checks its CommonJS interop at load time.
import path from "node:path";

const input = process.env.VCS_CONSUMER_BUNDLE_INPUT;
const output = process.env.VCS_CONSUMER_BUNDLE_OUTPUT;

if (!input) {
  throw new Error("VCS_CONSUMER_BUNDLE_INPUT is required");
}

if (!output) {
  throw new Error("VCS_CONSUMER_BUNDLE_OUTPUT is required");
}

export default {
  input,
  output: {
    file: output,
    format: "cjs",
    // Vite builds with the es2015 preset, whose namespace interop walks a module with for...in.
    generatedCode: "es2015",
  },
  external: (id) => !id.startsWith(".") && !path.isAbsolute(id),
  onwarn(warning, defaultHandler) {
    if (warning.code === "MISSING_EXPORT") {
      throw new Error(warning.message);
    }

    defaultHandler(warning);
  },
};
