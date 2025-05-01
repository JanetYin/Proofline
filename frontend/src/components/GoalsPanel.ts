import { Goal, ContextItem, Hole } from '../models/ProofState';

export class GoalsPanel {
  private container: HTMLElement;
  private currentGoal: Goal | null = null;
  private currentHole: Hole | null = null;

  constructor(containerId: string) {
    const element = document.getElementById(containerId);
    if (!element) {
      throw new Error(`Container element with ID ${containerId} not found`);
    }
    this.container = element;
  }

  /**
   * Update the goals panel with a new goal
   * @param goal The current goal to display
   * @param hole The original hole object (optional, for displaying code)
   */
  updateGoal(goal: Goal | null, hole?: Hole): void {
    console.log("Updating goal:", goal); // Debug log
    this.currentGoal = goal;
    this.currentHole = hole || null;
    this.render();
  }

  /**
   * Clear the goal panel
   */
  clear(): void {
    this.currentGoal = null;
    this.currentHole = null;
    this.render();
  }

  /**
   * Render the current goal and context
   */
  private render(): void {
    // Clear existing content
    this.container.innerHTML = '';
    
    if (!this.currentGoal) {
      this.renderEmptyState();
      return;
    }
    
    // Create goal panel structure with a modern design
    const goalContainer = document.createElement('div');
    goalContainer.className = 'goal-container';
    
    // Create goal header
    const header = document.createElement('div');
    header.className = 'goal-header';
    header.innerHTML = `<h4>#${this.currentGoal.holeId}</h4>`;
    goalContainer.appendChild(header);
    
    // Check if hole is solved - different displays for solved vs. unsolved
    const isSolved = this.currentHole?.isSolved || false;
    
    if (isSolved) {
      // For solved holes, show the value
      const valueSection = document.createElement('div');
      valueSection.className = 'goal-type-section';
      valueSection.innerHTML = `
        <h4>Value</h4>
        <div class="goal-type">${this.escapeHtml(this.currentHole?.value || '')}</div>
      `;
      goalContainer.appendChild(valueSection);
    } else {
      // For unsolved holes, show the type
      const typeSection = document.createElement('div');
      typeSection.className = 'goal-type-section';
      typeSection.innerHTML = `
        <h4>Type</h4>
        <div class="goal-type">${this.escapeHtml(this.currentGoal.type || 'Unknown type')}</div>
      `;
      goalContainer.appendChild(typeSection);
    }
    
    // Debug log to see if codeContext exists
    console.log(`Rendering goal with codeContext: ${this.currentGoal.codeContext ? 'Yes' : 'No'}`);
    
    // Always show code context if available (for both solved and unsolved holes)
    if (this.currentGoal.codeContext) {
      const codeSection = document.createElement('div');
      codeSection.className = 'goal-code-section';
      codeSection.innerHTML = `
        <h4>In</h4>
        <div class="goal-code">
          <pre>${this.escapeHtml(this.currentGoal.codeContext)}</pre>
        </div>
      `;
      goalContainer.appendChild(codeSection);
    }
    // Otherwise show basic location info if available from the hole
    else if (this.currentHole && this.currentHole.location) {
      const codeSection = document.createElement('div');
      codeSection.className = 'goal-code-section';
      codeSection.innerHTML = `
        <h4>Code Location</h4>
        <div class="goal-code">
          <div class="code-location">Line: ${this.currentHole.location.startLine}, Column: ${this.currentHole.location.startColumn}</div>
          <div class="code-snippet">Hole to fill: <code>${this.escapeHtml(this.currentGoal.codeRepresentation || `{?${this.currentGoal.holeId}}`)}</code></div>
        </div>
      `;
      goalContainer.appendChild(codeSection);
    }
    
    // Only show context for unsolved holes
    if (!isSolved) {
      // Create context section
      const contextSection = document.createElement('div');
      contextSection.className = 'context-section';
      contextSection.innerHTML = '<h4>Context</h4>';
      
      // Ensure context exists and is not null
      if (this.currentGoal.context && Array.isArray(this.currentGoal.context) && this.currentGoal.context.length > 0) {
        console.log("Rendering context with", this.currentGoal.context.length, "items");
        
        const contextTable = document.createElement('table');
        contextTable.className = 'context-table';
        
        // Add table header
        const tableHeader = document.createElement('thead');
        tableHeader.innerHTML = `
          <tr>
            <th>Name</th>
            <th>Type</th>
          </tr>
        `;
        contextTable.appendChild(tableHeader);
        
        // Add table body with context items
        const tableBody = document.createElement('tbody');
        
        // IMPORTANT: Display context items in the order they come from the API
        this.currentGoal.context.forEach((item: any) => {
          const row = document.createElement('tr');
          
          // Handle different context formats
          if (typeof item === 'object' && item !== null && item.name && item.type) {
            // Object format: { name: string, type: string }
            row.innerHTML = `
              <td>${this.escapeHtml(item.name)}</td>
              <td>${this.escapeHtml(item.type)}</td>
            `;
          } else if (typeof item === 'string') {
            // String format like: "id : (A : U) → A → A"
            const parts = item.split(' : ');
            if (parts.length >= 2) {
              const name = parts[0].trim();
              const type = parts.slice(1).join(' : ').trim();
              row.innerHTML = `
                <td>${this.escapeHtml(name)}</td>
                <td>${this.escapeHtml(type)}</td>
              `;
            } else {
              row.innerHTML = `
                <td colspan="2">${this.escapeHtml(String(item))}</td>
              `;
            }
          } else {
            // Fallback for unknown format
            row.innerHTML = `
              <td colspan="2">${this.escapeHtml(String(item))}</td>
            `;
          }
          
          tableBody.appendChild(row);
        });
        
        contextTable.appendChild(tableBody);
        contextSection.appendChild(contextTable);
      } else {
        // If no context, show a message
        contextSection.innerHTML += '<p class="no-context">No context variables available</p>';
      }
      
      goalContainer.appendChild(contextSection);
    }
    
    this.container.appendChild(goalContainer);
  }

  /**
   * Render the empty state
   */
  private renderEmptyState(): void {
    // Check if there are any unsolved holes by dispatching an event to get the status
    const event = new CustomEvent('check-all-holes-solved', {
      detail: { callback: (allSolved: boolean) => this.showEmptyOrCompleteState(allSolved) }
    });
    document.dispatchEvent(event);
  }
  
  /**
   * Show either empty state or completion state based on whether all holes are solved
   */
  private showEmptyOrCompleteState(allSolved: boolean): void {
    if (allSolved) {
      this.container.innerHTML = `
        <div class="complete-goal-state">
          <h3>All Done! 🎉</h3>
          <p>All holes are solved - proof is complete</p>
        </div>
      `;
    } else {
      this.container.innerHTML = `
        <div class="empty-goal-state">
          <p>No hole selected</p>
          <p>Click on a hole in the editor to view its goal</p>
        </div>
      `;
    }
  }

  /**
   * Escape HTML to prevent XSS
   * @param html The HTML string to escape
   */
  private escapeHtml(html: string): string {
    if (html === undefined || html === null) {
      return '';
    }
    const div = document.createElement('div');
    div.textContent = String(html);
    return div.innerHTML;
  }
}