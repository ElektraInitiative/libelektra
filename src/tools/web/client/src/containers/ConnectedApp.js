import { connect } from 'react-redux'

import App from '../components/App.jsx'

const mapStateToProps = (state) => {
  return state.router
}

const mapDispatchToProps = (dispatch) => {
  return {}
}

export default connect(mapStateToProps, mapDispatchToProps)(App)
