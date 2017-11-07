const React = require('react');
const createReactClass = require('create-react-class');
const MuiThemeProvider = require('material-ui/styles/MuiThemeProvider').default;

const MyMuiThemeProvider = createReactClass({
    render: function () {
        const props = this.props;
        // console.log("My MyMuiThemeProvider Props", props)
        return React.createElement(MuiThemeProvider, {},
                                   React.createElement("div", { className: 'MaterialUI' }, props.children)
                                  );
    }
});

exports.muiThemeProviderClass = MyMuiThemeProvider;
