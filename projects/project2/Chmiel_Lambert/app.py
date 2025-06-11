import pandas as pd
from dash import Dash, dcc, html, Input, Output
from plots import (
    make_plot_1, make_plot_2, make_plot_3,
    make_plot_4, make_plot_5, make_plot_6,
    make_plot_7, make_plot_8
)

# style dla nieaktywnej zakładki – z flexem dla centrowania

tab_style = {
    'backgroundColor': '#2E2E40',
    'color': '#F8F8F2',
    'border': 'none',
    'padding': '6px 12px',
    'fontFamily': 'Inter, sans-serif',
    'display': 'flex',
    'alignItems': 'center',
    'justifyContent': 'center'
}
# style dla aktywnej zakładki
tab_selected_style = {
    'backgroundColor': '#44475A',
    'color': '#F8F8F2',
    'fontWeight': '500',
    'display': 'flex',
    'alignItems': 'center',
    'justifyContent': 'center'
}

# Style for text sections
text_section_style = {
    'backgroundColor': '#2E2E40',
    'color': '#F8F8F2',
    'padding': '20px',
    'borderRadius': '8px',
    'fontFamily': 'Inter, sans-serif',
    'fontSize': '14px',
    'lineHeight': '1.6'
}

df = pd.read_excel('dane.xlsx')
df['Year']      = df['Treatment_Date'].dt.year
df['Month_Num'] = df['Treatment_Date'].dt.month
df['Month']     = df['Treatment_Date'].dt.month_name()

sports_groups = {
    "Top 6": df['Sport'].value_counts().nlargest(6).index.tolist(),
    "Druzynowe": ["SOCCER", "BASKETBALL", "VOLLEYBALL", "BASEBALL"],
    "Zimne": ["ICE SKATING", "SNOWBOARDING", "SNOW SKIING"],
    "Samotne": ["BICYCLES", "GOLF", "SCOOTERS", "WEIGHT LIFTING", "TENNIS", "SWIMMING"]
}

body_groups = {
    "Top 10 części ciała": df['Body_Part'].value_counts().nlargest(10).index.tolist(),
    "Ręce": ["SHOULDER", "UPPER ARM", "HAND", "LOWER ARM", "FINGER", "WRIST", "ELBOW"],
    "Nogi": ["TOE", "ANKLE", "KNEE", "FOOT", "LOWER LEG", "UPPER LEG"],
    "Głowa": ["FACE", "EAR", "HEAD", "NECK", "EYEBALL", "MOUTH"]
}

top_races = df['Race'].value_counts().nlargest(3).index.tolist()

app = Dash(__name__, suppress_callback_exceptions=True)

app.layout = html.Div(
    style={'backgroundColor': '#1E1E2F'},
    children=[
        html.H1(
            "Urazy w sporcie",
            style={
                'textAlign': 'center',
                'marginBottom': '30px',
                'color': '#F8F8F2',
                'fontFamily': 'Inter, sans-serif'
            }
        ),
        dcc.Tabs(
            id='tabs',
            value='tab-1',
            children=[
                dcc.Tab(label='Zakładka 1', value='tab-1', style=tab_style, selected_style=tab_selected_style),
                dcc.Tab(label='Zakładka 2', value='tab-2', style=tab_style, selected_style=tab_selected_style),
                dcc.Tab(label='Zakładka 3', value='tab-3', style=tab_style, selected_style=tab_selected_style),
                dcc.Tab(label='Zakładka 4', value='tab-4', style=tab_style, selected_style=tab_selected_style),
            ],
            style={'fontFamily': 'Inter, sans-serif'}
        ),
        html.Div(id='tabs-content', style={'padding': '20px'})
    ]
)

@app.callback(
    Output('tabs-content', 'children'),
    Input('tabs', 'value')
)
def render_tab(tab):
    if tab == 'tab-1':
        return html.Div([
            dcc.Graph(figure=make_plot_1(df, sports_groups), config={'displayModeBar': False}),
            html.Div([
                html.Ul([
                    html.Li("Względem roku 2020 ilość urazów związanych ze sportami drużynowymi znacznie zmalała. Prawdopodobnie jest to spowodowane spadkiem popularności tych dyscyplin z powodu pandemii. "),
                    html.Li("We wszystkich latach rozkład ilości urazów związanych z pływaniem jest skoncentrowany w miesiącach letnich. Można stąd wywnioskować, że urazy te są raczej spowodowane rekreacyjnym pływaniem w otwartych zbiornikach."),
                    html.Li("W miesiącach zimowych odnotowujemy najmniej urazów w większości dyscyplin. Wyjątkiem są sporty zimowe, których rozkład urazów jest skoncentrowany w tych miesiącach."),
                ])
            ], style=text_section_style),

            dcc.Graph(figure=make_plot_2(df), config={'displayModeBar': False}),
            html.Div([
                html.Ul([
                    html.Li("Głowa to najczęstsza kontuzjowana część ciała – ok. 12-13 % urazów każdego roku. Z roku na rok coraz większy procent urazów stanowią urazy głowy."),
                    html.Li("Obserwujemy wzrost udziału kontuzji kolana na przestrzeni lat. Być może siedzący tryb życia powoduje większą podatność kolan na kontuzje."),
                    html.Li("Na przestrzeni lat nieznacznie maleją udziały kontuzji nadgarstka oraz niższych części nóg."),
                ])
            ], style=text_section_style)
        ], style={'display': 'flex', 'flexDirection': 'column', 'gap': '40px'})

    elif tab == 'tab-2':
        return html.Div([
            dcc.Graph(figure=make_plot_3(df, sports_groups), config={'displayModeBar': False}),
            html.Div([
                html.Ul([
                    html.Li("Wśród najpopularniejszych sportów najwięcej urazów generuje koszykówka. Wśród osób czarnoskórych jest to prawie 40%, co wskazuje na szczególną popularność tego sportu w tej grupie."),
                    html.Li("Więcej niż co czwarty uraz u osoby białej jest związany z jazdą na rowerze. Możliwe, że sport ten jest szczególnie popularny wśród tych osób lub nie przykładają one dostatecznej wagi do bezpieczeństwa."),
                    html.Li("Urazy związane ze sportami zimowymi stanowią mały odsetek urazów w grupie osób czarnoskórych, w sumie mniej niż 1%."),
                ])
            ], style=text_section_style),

            dcc.Graph(id='heatmap-4', config={'displayModeBar': False}),
            html.Div([
                html.Div([
                    html.Label("Części ciała:"),
                    dcc.Dropdown(
                        id='body-dropdown',
                        options=[{"label": k, "value": k} for k in body_groups],
                        value=list(body_groups)[0],
                        clearable=False
                    )
                ], className='control'),
                html.Div([
                    html.Label("Sporty:"),
                    dcc.Dropdown(
                        id='sport-dropdown',
                        options=[{"label": k, "value": k} for k in sports_groups],
                        value=list(sports_groups)[0],
                        clearable=False
                    )
                ], className='control'),
                html.Div([
                    html.Label("Rasa:"),
                    dcc.RadioItems(
                        id='race-radio',
                        options=[{"label": r, "value": r} for r in top_races],
                        value=top_races[0],
                        labelStyle={'display': 'inline-block'}
                    )
                ], className='control')
            ], className='control-row'),
            html.Div([
                html.Ul([
                    html.Li("Niezależnie od rasy w koszykówce i siatkówce najczęstsze są kontuzje kostki. "),
                    html.Li("Widać dużą różnicę w rozkładzie urazów względem części ciała w narciarstwie i snowboardzie. Sporty te mogą wydawać się podobne, ale w rzeczywistości tak nie jest."),
                    html.Li("W wielu kategoriach udział urazów jest bardzo podobny między rasami. Są jednak wyjątki, przykładowo osoby białe częściej doznają urazów ramion przy podnoszeniu ciężarów, niż osoby czarnoskóre. Może to wskazywać na inne specyfiki treningów pomiędzy tymi rasami."),
                ])
            ], style=text_section_style)
        ], style={'display': 'flex', 'flexDirection': 'column', 'gap': '30px'})

    elif tab == 'tab-3':
        return html.Div([
            dcc.Graph(figure=make_plot_5(df), config={'displayModeBar': False}),
            html.Div([
                html.Ul([html.Li("W większości sportów to mężczyźni doznają więcej urazów. Można pomyśleć, że jest to spowodowane większą ostrożnością kobiet, ale kluczowym czynnikiem jest raczej większa liczba mężczyzn uprawiających analizowane dyscypliny."),
                    html.Li("W siatkówce i tańcu to kobiety doznają więcej urazów, a w pływaniu liczba urazów kobiet i mężczyzn jest podobna.")
                ])
            ], style=text_section_style),

            dcc.Graph(figure=make_plot_6(df), config={'displayModeBar': False}),
            html.Div([
                html.Ul([
                    html.Li("Wraz z wiekiem znacznie rośnie liczba urazów związanych z kolarstwem oraz maleje liczba urazów w pozostałych dyscyplinach, w szczególności drużynowych."),
                    html.Li("Największą różnorodność uprawianych sportów obserwujemy w grupie 20–30 lat, jednak nadal kolarstwo pozostaje najpopularniejsze."),                ])
            ], style=text_section_style)
        ], style={'display': 'flex', 'flexDirection': 'column', 'gap': '40px'})

    else:
        return html.Div([
            dcc.Graph(figure=make_plot_7(df), config={'displayModeBar': False}),
            html.Div([
                html.Ul([
                    html.Li("Obserwujemy wzrost liczby urazów na przestrzeni lat zarówno u kobiet jak i mężczyzn."),
                    html.Li("W 2022 obserwujemy ciekawą anomalię - ilość wypadków pod narkotyków i alkoholu odrobinę zmalała."),
                    html.Li("Wzrost całkowitej liczby urazów jest większy, niż liczby urazów pod wpływem alkoholu czy narkotyków, co może pokazywać większą świadomość ludzi odnośnie ryzyka związanego z zażywaniem tego typu substancji."),
                ])
            ], style=text_section_style),

            dcc.Graph(figure=make_plot_8(df), config={'displayModeBar': False}),
            html.Div([
                html.Ul([
                    html.Li("Łatwo można sprawdzić, że w kolarstwie alkohol i narkowtyki wpływa na ok. 5 % urazów."),
                    html.Li("Obserwujemy znacznie większy udział wypadków pod wpływem alkoholu/narkotyków w czterech sportach: kolarstwo, jazda na hulajnodze, taniec oraz golf. O ile w przypadku dwóch pierwszych sportów wynika to z faktu, że wymagają prowadzenia pojazdu, to w pozostałych przypadkach raczej jest to spowodowane okolicznościami w jakich uprawiany jest dany sport. Na przykład ludzie tańczą na imprezach, często po alkoholu."),
                    html.Li("W większości sportów wypadki pod wpływem substancji odurzających stanowią znaczną mniejszość."),
                ])
            ], style=text_section_style)
        ], style={'display': 'flex', 'flexDirection': 'column', 'gap': '40px'})

@app.callback(
    Output('heatmap-4', 'figure'),
    Input('race-radio', 'value'),
    Input('body-dropdown', 'value'),
    Input('sport-dropdown', 'value')
)
def update_heatmap4(selected_race, selected_bg, selected_sg):
    return make_plot_4(df, sports_groups, selected_race, selected_bg, selected_sg)

if __name__ == '__main__':
    app.run_server(debug=False)
