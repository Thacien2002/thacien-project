/* ==============================================================================
   🔹 MODERN SIDEBAR NAVIGATION JAVASCRIPT
   ============================================================================== */

class ModernSidebar {
  constructor() {
    this.sidebar = null;
    this.toggleButton = null;
    this.mainContent = null;
    this.isCollapsed = false;
    this.isMobile = window.innerWidth <= 768;
    this.animationDuration = 300; // ms
    
    this.init();
  }

  init() {
    // Wait for DOM to be ready
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', () => this.setupSidebar());
    } else {
      this.setupSidebar();
    }
  }

  setupSidebar() {
    // Create sidebar container
    this.createSidebarHTML();
    
    // Bind events
    this.bindEvents();
    
    // Initialize state
    this.updateSidebarState();
    
    // Handle responsive behavior
    this.handleResponsive();
    
    // Listen for Shiny custom messages
    this.setupShinyIntegration();
  }

  createSidebarHTML() {
    // Create sidebar container
    const sidebarHTML = `
      <div class="sidebar-container" id="modern-sidebar">
        <div class="sidebar-header">
          <a href="#" class="sidebar-logo">
            <div class="sidebar-logo-icon">
              <i class="fas fa-chart-line"></i>
            </div>
            <span class="sidebar-logo-text">Rwanda Nutrition</span>
          </a>
          <button class="sidebar-toggle" id="sidebar-toggle" aria-label="Toggle navigation">
            <i class="fas fa-bars"></i>
          </button>
        </div>
        
        <nav class="sidebar-nav">
          <ul class="sidebar-menu">
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="language" data-tooltip="Language">
                <i class="sidebar-menu-icon fas fa-globe"></i>
                <span class="sidebar-menu-text">Language</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="overview" data-tooltip="National Overview">
                <i class="sidebar-menu-icon fas fa-flag"></i>
                <span class="sidebar-menu-text">National Overview</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="rankings" data-tooltip="District Rankings">
                <i class="sidebar-menu-icon fas fa-trophy"></i>
                <span class="sidebar-menu-text">District Rankings</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="regional" data-tooltip="Regional Comparison">
                <i class="sidebar-menu-icon fas fa-globe-africa"></i>
                <span class="sidebar-menu-text">Regional Comparison</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="trends" data-tooltip="Trend Analysis">
                <i class="sidebar-menu-icon fas fa-chart-line"></i>
                <span class="sidebar-menu-text">Trend Analysis</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="predictions" data-tooltip="Predictive Analytics">
                <i class="sidebar-menu-icon fas fa-flask"></i>
                <span class="sidebar-menu-text">Predictive Analytics</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="policy" data-tooltip="Policy Advisor">
                <i class="sidebar-menu-icon fas fa-lightbulb"></i>
                <span class="sidebar-menu-text">Policy Advisor</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="explorer" data-tooltip="Data Explorer">
                <i class="sidebar-menu-icon fas fa-search"></i>
                <span class="sidebar-menu-text">Data Explorer</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="impact" data-tooltip="Impact Assessment">
                <i class="sidebar-menu-icon fas fa-medal"></i>
                <span class="sidebar-menu-text">Impact Assessment</span>
              </a>
            </li>
            <li class="sidebar-menu-item">
              <a href="#" class="sidebar-menu-link" data-tab="reports" data-tooltip="Export & Reports">
                <i class="sidebar-menu-icon fas fa-download"></i>
                <span class="sidebar-menu-text">Export & Reports</span>
              </a>
            </li>
          </ul>
        </nav>
        
        <div class="sidebar-filters">
          <h5 class="sidebar-filters-title">Global Filters</h5>
          
          <div class="sidebar-filter-group">
            <label class="sidebar-filter-label">Year</label>
            <select class="sidebar-filter-input" id="filter-year">
              <option value="2020">2020</option>
              <option value="2019">2019</option>
              <option value="2018">2018</option>
              <option value="2017">2017</option>
              <option value="2016">2016</option>
            </select>
          </div>
          
          <div class="sidebar-filter-group">
            <label class="sidebar-filter-label">Province</label>
            <select class="sidebar-filter-input" id="filter-province">
              <option value="all">All Provinces</option>
              <option value="Kigali">Kigali</option>
              <option value="Northern">Northern</option>
              <option value="Southern">Southern</option>
              <option value="Eastern">Eastern</option>
              <option value="Western">Western</option>
            </select>
          </div>
          
          <div class="sidebar-filter-group">
            <label class="sidebar-filter-label">Urwego rw'amavuko</label>
            <select class="sidebar-filter-input" id="filter-age-group">
              <option value="all">All Age Groups</option>
              <option value="0-6 months">0-6 months</option>
              <option value="6-23 months">6-23 months</option>
              <option value="24-59 months">24-59 months</option>
            </select>
          </div>
          
          <div class="sidebar-filter-group">
            <label class="sidebar-filter-label">Ibiribwa by'ingenzi</label>
            <select class="sidebar-filter-input" id="filter-nutrient">
              <option value="all">All Nutrients</option>
              <option value="Iron">Iron</option>
              <option value="Vitamin A">Vitamin A</option>
              <option value="Zinc">Zinc</option>
              <option value="Protein">Protein</option>
            </select>
          </div>
          
          <div class="sidebar-filter-group">
            <label class="sidebar-filter-label">Ibimenyetso</label>
            <div class="sidebar-checkbox-group">
              <label class="sidebar-checkbox-item">
                <input type="checkbox" id="indicator-stunting" value="stunting" checked>
                <span>Gutinda kukura</span>
              </label>
              <label class="sidebar-checkbox-item">
                <input type="checkbox" id="indicator-wasting" value="wasting" checked>
                <span>Guhira</span>
              </label>
              <label class="sidebar-checkbox-item">
                <input type="checkbox" id="indicator-anemia" value="anemia" checked>
                <span>Kurwara amaraso make</span>
              </label>
            </div>
          </div>
        </div>
      </div>
    `;

    // Insert sidebar into the page
    document.body.insertAdjacentHTML('afterbegin', sidebarHTML);
    
    // Get references to elements
    this.sidebar = document.getElementById('modern-sidebar');
    this.toggleButton = document.getElementById('sidebar-toggle');
    this.mainContent = document.querySelector('.content-wrapper') || document.querySelector('.main-content');
    
    // Add main content class if it doesn't exist
    if (this.mainContent && !this.mainContent.classList.contains('main-content')) {
      this.mainContent.classList.add('main-content');
    }
  }

  bindEvents() {
    // Toggle button click
    this.toggleButton.addEventListener('click', (e) => {
      e.preventDefault();
      this.toggleSidebar();
    });

    // Menu item clicks
    const menuLinks = this.sidebar.querySelectorAll('.sidebar-menu-link');
    menuLinks.forEach(link => {
      link.addEventListener('click', (e) => {
        e.preventDefault();
        this.handleMenuClick(link);
      });
    });

    // Filter changes
    const filterInputs = this.sidebar.querySelectorAll('.sidebar-filter-input, .sidebar-checkbox-group input');
    filterInputs.forEach(input => {
      input.addEventListener('change', () => {
        this.handleFilterChange();
      });
    });

    // Keyboard navigation
    document.addEventListener('keydown', (e) => {
      if (e.ctrlKey && e.key === 'b') {
        e.preventDefault();
        this.toggleSidebar();
      }
    });

    // Window resize
    window.addEventListener('resize', () => {
      this.handleResponsive();
    });

    // Click outside to close on mobile
    document.addEventListener('click', (e) => {
      if (this.isMobile && !this.sidebar.contains(e.target) && !this.isCollapsed) {
        this.collapseSidebar();
      }
    });
  }

  toggleSidebar() {
    if (this.isCollapsed) {
      this.expandSidebar();
    } else {
      this.collapseSidebar();
    }
  }

  expandSidebar() {
    this.isCollapsed = false;
    this.sidebar.classList.remove('collapsed');
    
    if (this.mainContent) {
      this.mainContent.classList.remove('sidebar-collapsed');
    }
    
    // Update toggle button icon
    this.toggleButton.innerHTML = '<i class="fas fa-bars"></i>';
    
    // Animate text elements
    this.animateTextElements('fade-in');
    
    // Store state
    localStorage.setItem('sidebar-collapsed', 'false');
    
    // Trigger custom event
    this.dispatchEvent('sidebar-expanded');
  }

  collapseSidebar() {
    this.isCollapsed = true;
    this.sidebar.classList.add('collapsed');
    
    if (this.mainContent) {
      this.mainContent.classList.add('sidebar-collapsed');
    }
    
    // Update toggle button icon
    this.toggleButton.innerHTML = '<i class="fas fa-bars"></i>';
    
    // Animate text elements
    this.animateTextElements('fade-out');
    
    // Store state
    localStorage.setItem('sidebar-collapsed', 'true');
    
    // Trigger custom event
    this.dispatchEvent('sidebar-collapsed');
  }

  animateTextElements(animationClass) {
    const textElements = this.sidebar.querySelectorAll('.sidebar-menu-text, .sidebar-filters-title, .sidebar-filter-label');
    
    textElements.forEach(element => {
      element.classList.remove('fade-in', 'fade-out');
      // Force reflow
      element.offsetHeight;
      element.classList.add(animationClass);
    });
  }

  handleMenuClick(link) {
    // Remove active class from all links
    const allLinks = this.sidebar.querySelectorAll('.sidebar-menu-link');
    allLinks.forEach(l => l.classList.remove('active'));
    
    // Add active class to clicked link
    link.classList.add('active');
    
    // Get tab name
    const tabName = link.dataset.tab;
    
    // Trigger Shiny tab change if available
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue('sidebar_tab_change', tabName, {priority: 'event'});
    }
    
    // Collapse sidebar on mobile after selection
    if (this.isMobile) {
      setTimeout(() => this.collapseSidebar(), 300);
    }
    
    // Trigger custom event
    this.dispatchEvent('menu-item-selected', { tabName });
  }

  handleFilterChange() {
    const filters = {
      year: document.getElementById('filter-year')?.value,
      province: document.getElementById('filter-province')?.value,
      ageGroup: document.getElementById('filter-age-group')?.value,
      nutrient: document.getElementById('filter-nutrient')?.value,
      indicators: Array.from(document.querySelectorAll('.sidebar-checkbox-group input:checked')).map(cb => cb.value)
    };
    
    // Trigger Shiny input update if available
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue('sidebar_filters', filters, {priority: 'event'});
    }
    
    // Trigger custom event
    this.dispatchEvent('filters-changed', { filters });
  }

  handleResponsive() {
    const wasMobile = this.isMobile;
    this.isMobile = window.innerWidth <= 768;
    
    if (wasMobile !== this.isMobile) {
      if (this.isMobile) {
        // Mobile: start collapsed
        this.sidebar.classList.add('collapsed');
        this.isCollapsed = true;
      } else {
        // Desktop: restore saved state
        const savedState = localStorage.getItem('sidebar-collapsed');
        if (savedState === 'true') {
          this.collapseSidebar();
        } else {
          this.expandSidebar();
        }
      }
    }
  }

  updateSidebarState() {
    // Restore saved state on desktop
    if (!this.isMobile) {
      const savedState = localStorage.getItem('sidebar-collapsed');
      if (savedState === 'true') {
        this.collapseSidebar();
      }
    } else {
      // Mobile: always start collapsed
      this.collapseSidebar();
    }
  }

  dispatchEvent(eventName, detail = {}) {
    const event = new CustomEvent(`sidebar:${eventName}`, {
      detail: detail,
      bubbles: true
    });
    document.dispatchEvent(event);
  }

  setupShinyIntegration() {
    // Listen for Shiny custom messages
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler('updateSidebarFilters', (message) => {
        this.updateFilters(message);
      });
      
      window.Shiny.addCustomMessageHandler('setActiveTab', (message) => {
        this.setActiveTab(message.tabName);
      });
    }
  }

  // Public methods for external control
  setActiveTab(tabName) {
    const link = this.sidebar.querySelector(`[data-tab="${tabName}"]`);
    if (link) {
      this.handleMenuClick(link);
    }
  }

  updateFilters(filters) {
    // Update filter inputs with new values
    if (filters.year) {
      const yearSelect = document.getElementById('filter-year');
      if (yearSelect) yearSelect.value = filters.year;
    }
    
    if (filters.province) {
      const provinceSelect = document.getElementById('filter-province');
      if (provinceSelect) provinceSelect.value = filters.province;
    }
    
    if (filters.ageGroup) {
      const ageSelect = document.getElementById('filter-age-group');
      if (ageSelect) ageSelect.value = filters.ageGroup;
    }
    
    if (filters.nutrient) {
      const nutrientSelect = document.getElementById('filter-nutrient');
      if (nutrientSelect) nutrientSelect.value = filters.nutrient;
    }
    
    if (filters.indicators) {
      const checkboxes = document.querySelectorAll('.sidebar-checkbox-group input');
      checkboxes.forEach(cb => {
        cb.checked = filters.indicators.includes(cb.value);
      });
    }
  }

  getCurrentFilters() {
    return {
      year: document.getElementById('filter-year')?.value,
      province: document.getElementById('filter-province')?.value,
      ageGroup: document.getElementById('filter-age-group')?.value,
      nutrient: document.getElementById('filter-nutrient')?.value,
      indicators: Array.from(document.querySelectorAll('.sidebar-checkbox-group input:checked')).map(cb => cb.value)
    };
  }
}

// Initialize sidebar when script loads
let modernSidebar;

// Initialize immediately if DOM is ready, otherwise wait
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => {
    modernSidebar = new ModernSidebar();
  });
} else {
  modernSidebar = new ModernSidebar();
}

// Make sidebar globally accessible
window.ModernSidebar = ModernSidebar;
window.modernSidebar = modernSidebar;

// Export for module systems
if (typeof module !== 'undefined' && module.exports) {
  module.exports = ModernSidebar;
}
