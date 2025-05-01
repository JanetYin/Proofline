import { Hole, CodeLocation, ContextItem } from '../models/ProofState';

/**
 * Find hole locations in code by scanning for explicit hole references
 * @param code The source code to scan
 * @param holes The hole data from the API
 * @returns Holes with added location information
 */
export function findHoleLocations(code: string, holes: Hole[]): Hole[] {
  if (!holes || !Array.isArray(holes)) {
    return [];
  }
  
  // Clone the holes to avoid modifying the original
  const holesWithLocations = holes.map(hole => ({...hole}));
  
  // Track which holes have been assigned locations
  const assignedHoles = new Set<number | string>();
  
  // Convert code to lines for easier processing
  const lines = code.split('\n');
  
  // Find explicit hole references like {?0}, {?1}, etc.
  findExplicitHoles(lines, holesWithLocations, assignedHoles);
  
  // For any remaining holes without location, assign default position
  assignDefaultLocations(holesWithLocations, assignedHoles);
  
  return holesWithLocations;
}

/**
 * Find explicit hole references like {?0}, {?1}, etc.
 */
function findExplicitHoles(
  lines: string[], 
  holes: Hole[], 
  assignedHoles: Set<number | string>
): void {
  // Regular expression for bracketed holes: {?0}, {?1}, etc.
  const bracketHoleRegex = /\{[^}]*\?(\d+)[^}]*\}/g;
  
  lines.forEach((line, lineIndex) => {
    // Need to reset the regex for each line
    bracketHoleRegex.lastIndex = 0;
    
    let match;
    while ((match = bracketHoleRegex.exec(line)) !== null) {
      const holeId = Number(match[1]);
      const startColumn = match.index + 1; // +1 for 1-based indexing
      const endColumn = startColumn + match[0].length;
      
      // Find the hole with this exact ID
      const hole = holes.find(h => {
        const hId = typeof h.holeId === 'number' ? h.holeId : parseInt(String(h.holeId), 10);
        return hId === holeId;
      });
      
      if (hole) {
        hole.location = {
          startLine: lineIndex + 1, // +1 for 1-based indexing
          startColumn: startColumn,
          endLine: lineIndex + 1,
          endColumn: endColumn
        };
        assignedHoles.add(hole.holeId);
      }
    }
  });
}

/**
 * Assign default locations to any holes without locations
 */
function assignDefaultLocations(
  holes: Hole[], 
  assignedHoles: Set<number | string>
): void {
  holes.forEach(hole => {
    if (!assignedHoles.has(hole.holeId)) {
      hole.location = {
        startLine: 1,
        startColumn: 1,
        endLine: 1,
        endColumn: 2
      };
    }
  });
}

/**
 * Build a formatted context string like "name : type" from context items
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