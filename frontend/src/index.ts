import { ProoflineApp } from './app';
import './styles/main.css';

/**
 * Global error handler
 */
window.addEventListener('error', (event) => {
  console.error('Caught runtime error:', event.error);
  
  const statusBar = document.getElementById('status-message');
  if (statusBar) {
    statusBar.textContent = `Error: ${event.error.message}`;
    statusBar.style.color = 'var(--danger-color)';
  }
  
  event.preventDefault();
});

/**
 * Handle unhandled promise rejections
 */
window.addEventListener('unhandledrejection', (event) => {
  console.error('Unhandled promise rejection:', event.reason);
  
  const statusBar = document.getElementById('status-message');
  if (statusBar) {
    statusBar.textContent = `Error: ${event.reason.message || 'Unhandled promise rejection'}`;
    statusBar.style.color = 'var(--danger-color)';
  }
  
  event.preventDefault();
});

/**
 * Application initialization
 */
document.addEventListener('DOMContentLoaded', () => {
  // Display loading message
  const statusBar = document.getElementById('status-message');
  if (statusBar) {
    statusBar.textContent = 'Initializing Proofline...';
  }
  
  // Show loading spinner
  const loadingSpinner = document.getElementById('loading-spinner');
  if (loadingSpinner) {
    loadingSpinner.style.display = 'inline-block';
  }
  
  // Initialize the application
  const app = new ProoflineApp();
  app.init().catch(err => {
    console.error('Failed to initialize app:', err);
    
    if (statusBar) {
      statusBar.textContent = `Failed to initialize: ${err.message}`;
      statusBar.style.color = 'var(--danger-color)';
    }
    
    if (loadingSpinner) {
      loadingSpinner.style.display = 'none';
    }
    
    // Show error alert
    alert(`Failed to initialize Proofline: ${err.message}\n\nPlease check the console for more details.`);
  });
});