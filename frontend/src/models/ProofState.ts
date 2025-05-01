/**
 * Simplified ProofState.ts
 * Removed unused interfaces and simplified the remaining ones
 */

/**
 * Represents a location in the code
 */
export interface CodeLocation {
  startLine: number;
  startColumn: number;
  endLine: number;
  endColumn: number;
}

/**
 * Represents a context item (variable and its type)
 */
export interface ContextItem {
  name: string;
  type: string;
}

/**
 * Represents a metavariable (hole) in the proof
 */
export interface Hole {
  holeId: string | number;
  isSolved: boolean;
  expectedType?: string;     // Type for unsolved holes
  value?: string;            // Value for solved holes
  context?: (ContextItem | string)[]; // Context for unsolved holes
  location?: CodeLocation;   // Location in the code
  codeContext?: string;      // Code context extracted from term string
}

/**
 * Represents the current state of the proof
 */
export interface ProofState {
  script: string;
  fileName?: string;
  holes: Hole[];
  errorMessage?: string;
}

/**
 * Represents a goal to display in the goals panel
 */
export interface Goal {
  holeId: string | number;
  type: string;
  context: (ContextItem | string)[];
  // Code representation of the hole in the normalized form {?n}
  codeRepresentation?: string;
  // Full code context showing surrounding lines
  codeContext?: string;
}

/**
 * API Response from check endpoint
 */
export interface CheckResponse {
  success: boolean;
  message?: string;
  term?: string;
  type?: string;
  state?: ProofState;
}

/**
 * Request to save a file
 */
export interface SaveFileRequest {
  fileName: string;
  content: string;
}