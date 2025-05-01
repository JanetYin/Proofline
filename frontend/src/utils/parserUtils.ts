import { Hole, ContextItem } from '../models/ProofState';

/**
 * Utility functions for parsing and formatting in the application
 */

/**
 * Extract context information for each hole from a term string
 * @param termString The term string containing hole references
 * @returns A map of hole IDs to their code context
 */
export function extractHoleContexts(termString: string): Map<string, string> {
  const holeContexts = new Map<string, string>();
  
  if (!termString) {
    return holeContexts;
  }
  
  // Split the term string into lines for processing
  const lines = termString.split('\n');
  
  // Current context being built
  let currentContext = '';
  let currentHoleIds: string[] = [];
  
  // Process each line
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    
    // Skip empty lines that separate declarations
    if (line.trim() === '') {
      // If we've been building a context and found hole IDs, save it
      if (currentHoleIds.length > 0 && currentContext.trim() !== '') {
        for (const holeId of currentHoleIds) {
          holeContexts.set(holeId, currentContext.trim());
        }
        
        // Reset for next context
        currentContext = '';
        currentHoleIds = [];
      }
      continue;
    }
    
    // Add line to current context
    currentContext += line + '\n';
    
    // Extract hole IDs from the current line using regex
    const holeMatches = line.match(/\{(\?\d+)\}/g);
    if (holeMatches) {
      for (const match of holeMatches) {
        // Extract just the number from {?n}
        const holeId = match.substring(2, match.length - 1);
        if (!currentHoleIds.includes(holeId)) {
          currentHoleIds.push(holeId);
        }
      }
    }
    
    // If the line appears to end a declaration (ends with semicolon)
    // or if we're at the last line, save the current context
    const isLastLine = i === lines.length - 1;
    const isEndOfDeclaration = line.trim().endsWith(';');
    
    if ((isEndOfDeclaration || isLastLine) && currentHoleIds.length > 0) {
      for (const holeId of currentHoleIds) {
        holeContexts.set(holeId, currentContext.trim());
      }
      
      // Reset for next context
      currentContext = '';
      currentHoleIds = [];
    }
  }
  
  return holeContexts;
}

/**
 * Extract surrounding code context for a hole
 * @param script The full script text
 * @param hole The hole to get context for
 * @param lineContext Number of lines of context to include before and after
 * @returns A string with the surrounding code
 */
export function getHoleCodeContext(script: string, hole: Hole, lineContext: number = 1): string {
  if (!script || !hole || !hole.location) return '';
  
  const lines = script.split('\n');
  
  // Get location info
  const { startLine, endLine } = hole.location;
  
  // Calculate range of lines to include
  const startIdx = Math.max(0, startLine - lineContext - 1); // -1 because line numbers are 1-based
  const endIdx = Math.min(lines.length - 1, endLine + lineContext - 1);
  
  // Extract the line(s) containing the hole and surrounding context
  const codeLines = lines.slice(startIdx, endIdx + 1);
  
  // Build the code representation
  let codeContext = '';
  codeLines.forEach((line, idx) => {
    const lineNumber = startIdx + idx + 1;
    codeContext += `${lineNumber}: ${line}\n`;
  });
  
  return codeContext.trim();
}

/**
 * Parse error message to extract line and column information
 * @param errorMsg The error message to parse
 * @returns Parsed error information with optional line and column
 */
export function parseErrorMessage(errorMsg: string): { message: string, line?: number, column?: number } {
  // Try to match "(stdin):line:column:" format for error messages
  const pattern = /\(stdin\):(\d+):(\d+):/;
  const match = errorMsg.match(pattern);
  
  if (match) {
    const line = parseInt(match[1], 10);
    const column = parseInt(match[2], 10);
    
    return { 
      message: errorMsg,
      line,
      column
    };
  }
  
  return { message: errorMsg };
}

/**
 * Format error message as HTML for display
 * @param message Error message to format
 * @returns HTML formatted error message
 */
export function formatErrorAsHTML(message: string): string {
  // If already formatted, return as is
  if (message.startsWith('<div class="error-message">')) {
    return message;
  }
  
  // Check if it's a formatted error message
  if (message.includes("(stdin):") && message.includes(" | ")) {
    // Split error message lines
    const lines = message.split('\n');
    let html = '<div class="error-message">';
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // Handle location line
      if (line.match(/\(stdin\):\d+:\d+:/)) {
        html += `<div class="error-location">${escapeHtml(line)}</div>`;
      }
      // Handle code line
      else if (line.match(/^\s*\|\s/)) {
        html += `<div class="error-code-line">${escapeHtml(line)}</div>`;
      }
      // Handle pointer line (^)
      else if (line.includes('^')) {
        html += `<div class="error-pointer">${escapeHtml(line)}</div>`;
      }
      // Handle error description
      else {
        html += `<div class="error-description">${escapeHtml(line)}</div>`;
      }
    }
    
    html += '</div>';
    return html;
  }
  
  // Default format for simple error messages
  return `<div class="error-title">Error</div>
          <div class="error-message">${escapeHtml(message)}</div>`;
}

/**
 * Format a context item as a string
 * @param contextItem The context item to format
 * @returns Formatted context string
 */
export function formatContextItem(contextItem: ContextItem | string): string {
  if (typeof contextItem === 'string') {
    return contextItem;
  }
  
  if (contextItem.name && contextItem.type) {
    return `${contextItem.name} : ${contextItem.type}`;
  }
  
  return String(contextItem);
}

/**
 * Generate a code representation of a hole
 * @param hole The hole object
 * @return String representation of the hole in code format
 */
export function getHoleCodeRepresentation(hole: Hole): string {
  // Get hole ID in numeric form
  const holeId = typeof hole.holeId === 'string' ? parseInt(hole.holeId, 10) : hole.holeId;
  
  if (hole.isSolved) {
    // For solved holes, show the value if available
    return hole.value ? `{?${holeId} = ${hole.value}}` : `{?${holeId}}`;
  } else {
    // For unsolved holes, show in standardized format
    return `{?${holeId}}`;
  }
}

/**
 * Escape HTML to prevent XSS
 * @param text The text to escape
 * @returns Escaped HTML string
 */
export function escapeHtml(text: string): string {
  if (text === undefined || text === null) {
    return '';
  }
  
  return String(text)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;')
    .replace(/\n/g, '<br>')
    .replace(/ /g, '&nbsp;');
}