require('../styles/index.scss');

require('../src/Main.purs').main();

// If hot-reloading, hook into each state change and re-render using the last
// state.
if (module.hot) {
  module.hot.accept();
}
