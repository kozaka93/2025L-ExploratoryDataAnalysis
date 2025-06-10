import pandas as pd
import plotly.graph_objects as go
import numpy as np

# ==== GLOBAL DESIGN CONFIGURATION - ZMIANY ====

# Zaktualizowana, ciemna paleta tła oraz żywe kolory akcentowe
COLOR_PALETTE = {
    'pl1': '#FF5555',
    'pl2': '#50FA7B',
    'pl3': '#BD93F9',
    'pl4': '#FFB86C',
    'pl5': '#8BE9FD',
    'bg': '#1E1E2F',            # ciemne tło (grafitowo-granatowe)
    'text_primary': '#F8F8F2',  # jasny, prawie biały tekst
    'text_secondary': '#BFBFBF',# jasnoszary tekst
    'accent1': '#FF5555',       # żywa czerwień
    'accent2': '#50FA7B',       # neonowa zieleń
    'accent3': '#BD93F9',       # żywy fiolet
    'accent4': '#FFB86C',       # pomarańczowo-koralowy
    'accent5': '#8BE9FD',       # jasnoniebieski
    'neutral_light': '#44475A', # ciemny szary (siatka, obramowania)
    'neutral_dark': '#282A36'   # bardzo ciemny odcień do elementów pomocniczych
}

# Skala kolorów do heatmap: gradient o dwóch rozbieżnościach (dwukolorowy)
colorscale_heatmap = [
    [0.0, COLOR_PALETTE['neutral_dark']],
    [1.0, COLOR_PALETTE['accent1']]
]

# Bardziej żywe kolory do wykresów słupkowych
COLUMN_COLORS_BAR = [
    COLOR_PALETTE['accent1'],
    COLOR_PALETTE['accent2'],
    COLOR_PALETTE['accent3'],
    COLOR_PALETTE['accent4'],
    COLOR_PALETTE['accent5']
]

# Typography configuration (bez zmian)
FONT_CONFIG = {
    'title': dict(
        family="Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size=18,
        color=COLOR_PALETTE['text_primary']
    ),
    'axis_title': dict(
        family="Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size=14,
        color=COLOR_PALETTE['text_primary']
    ),
    'axis_labels': dict(
        family="Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size=12,
        color=COLOR_PALETTE['text_secondary']
    ),
    'legend': dict(
        family="Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
        size=12,
        color=COLOR_PALETTE['text_secondary']
    )
}

# Zaktualizowana funkcja apply_theme, aby obsługiwała ciemne tło
def apply_theme(fig, title, x_title="", y_title="", height=None, legend_side=False):
    layout_updates = {
        'title': dict(
            text=title,
            font=dict(
                family="Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
                size=18,
                color=COLOR_PALETTE['text_primary']
            ),
            x=0.02,
            xanchor='left'
        ),
        'xaxis': dict(
            title=dict(text=x_title, font=dict(size=14, color=COLOR_PALETTE['text_primary'])),
            tickfont=dict(size=12, color=COLOR_PALETTE['text_secondary']),
            gridcolor='rgba(68, 71, 90, 0.5)',
            linecolor=COLOR_PALETTE['neutral_light'],
            zerolinecolor=COLOR_PALETTE['neutral_light']
        ),
        'yaxis': dict(
            title=dict(text=y_title, font=dict(size=14, color=COLOR_PALETTE['text_primary'])),
            tickfont=dict(size=12, color=COLOR_PALETTE['text_secondary']),
            gridcolor='rgba(68, 71, 90, 0.5)',
            linecolor=COLOR_PALETTE['neutral_light'],
            zerolinecolor=COLOR_PALETTE['neutral_light']
        ),
        'plot_bgcolor': COLOR_PALETTE['bg'],
        'paper_bgcolor': COLOR_PALETTE['bg'],
        'font': dict(
            family="Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
            size=12,
            color=COLOR_PALETTE['text_secondary']
        ),
    }
    if legend_side:
        layout_updates['legend'] = dict(
            x=1.02, y=1, xanchor='left', yanchor='top',
            orientation='v',
            font=dict(size=12, color=COLOR_PALETTE['text_secondary']),
            bgcolor='rgba(40, 42, 54, 0.8)',
            bordercolor=COLOR_PALETTE['neutral_light'],
            borderwidth=1
        )
    else:
        layout_updates['legend'] = dict(
            font=dict(size=12, color=COLOR_PALETTE['text_secondary']),
            bgcolor='rgba(40, 42, 54, 0.8)',
            bordercolor=COLOR_PALETTE['neutral_light'],
            borderwidth=1
        )
    if height:
        layout_updates['height'] = height
    fig.update_layout(**layout_updates)
    return fig

def make_plot_1(df, sports_groups):
    years = sorted(df['Year'].unique())
    group_names = list(sports_groups.keys())

    all_group_data = {}
    global_max = 0

    for group_name, sports_list in sports_groups.items():
        yearly_vals = {}

        for yr in years:
            year_data = df[(df['Year'] == yr) & (df['Sport'].isin(sports_list))]
            if year_data.empty:
                pivot = pd.DataFrame(0, index=sports_list, columns=range(1, 13))
            else:
                pivot = (
                    year_data
                    .groupby(['Sport', 'Month_Num'])
                    .size()
                    .reset_index(name='count')
                    .pivot(index='Sport', columns='Month_Num', values='count')
                    .reindex(index=sports_list, columns=range(1, 13))  # bez fill_value, wypełnimy potem
                ).fillna(0)  # zastępujemy ewentualne NaN zerami

            vals = pivot.values.astype(float)

            for i in range(len(sports_list)):
                sport_total = vals[i].sum()
                if sport_total > 0:
                    vals[i] = (vals[i] / sport_total) * 100
                else:
                    vals[i] = 0.0

            yearly_vals[yr] = vals
            current_max = vals.max() if vals.size > 0 else 0
            if current_max > global_max:
                global_max = current_max

        all_group_data[group_name] = yearly_vals

    traces = []
    for group_idx, (group_name, sports_list) in enumerate(sports_groups.items()):
        for year_idx, yr in enumerate(years):
            is_visible = (group_idx == 0 and year_idx == 0)
            traces.append(go.Heatmap(
                z=all_group_data[group_name][yr],
                x=[pd.to_datetime(m, format='%m').strftime('%b') for m in range(1, 13)],
                y=sports_list,
                colorscale=colorscale_heatmap,
                zmin=0,
                zmax=global_max,
                coloraxis="coloraxis",
                visible=is_visible,
                hovertemplate='<b>%{y}</b><br>%{x}: <b>%{z:.1f}%</b> urazów<extra></extra>',
                name=f"{group_name}_{yr}"
            ))

    fig = go.Figure(data=traces)

    def create_slider_steps(selected_group_idx):
        steps = []
        for year_idx, yr in enumerate(years):
            visible = [False] * len(traces)
            trace_idx = selected_group_idx * len(years) + year_idx
            visible[trace_idx] = True
            steps.append({
                "method": "update",
                "label": str(yr),
                "args": [
                    {"visible": visible},
                    {"title": f"Sezonowość urazów: '{group_names[selected_group_idx]}' – {yr}<br><sub>procenty</sub>"}
                ]
            })
        return steps

    sliders = [dict(
        active=0,
        currentvalue={"prefix": "Rok: "},
        pad={"t": 50},
        steps=create_slider_steps(0),
        font=dict(size=12, color=COLOR_PALETTE['text_secondary'])
    )]

    dropdown_buttons = []
    for group_idx, group_name in enumerate(group_names):
        visible = [False] * len(traces)
        trace_idx = group_idx * len(years)
        visible[trace_idx] = True
        dropdown_buttons.append({
            "label": group_name,
            "method": "update",
            "args": [
                {"visible": visible},
                {
                    "title": f"Sezonowość urazów: '{group_name}' – {years[0]}<br><sub>procenty</sub>",
                    "sliders": [dict(
                        active=0,
                        currentvalue={"prefix": "Rok: "},
                        pad={"t": 50},
                        steps=create_slider_steps(group_idx),
                        font=dict(size=12, color=COLOR_PALETTE['text_secondary'])
                    )]
                }
            ]
        })

    fig.update_layout(
        updatemenus=[dict(
            buttons=dropdown_buttons,
            direction="down",
            pad={"r": 10, "t": 10},
            showactive=True,
            x=1.02,             
            xanchor="left",
            y=1.35,       
            yanchor="top",
            font=dict(size=12, color=COLOR_PALETTE['text_secondary']),
            bgcolor='rgba(40, 42, 54, 0.8)',
            bordercolor=COLOR_PALETTE['neutral_light'],
            borderwidth=1
        )],
        sliders=sliders,
        coloraxis=dict(
            colorscale=colorscale_heatmap,
            cmin=0,
            cmax=global_max,
            colorbar=dict(
                title="Procent urazów",
                titlefont=dict(size=14, color=COLOR_PALETTE['text_primary']),
                tickfont=dict(size=12, color=COLOR_PALETTE['text_secondary'])
            )
        ),
        margin=dict(l=200, t=140, r=200, b=80)  # zwiększone margin.r, by dropdown nie nachodził
    )

    return apply_theme(
        fig,
        f"Sezonowość urazów: '{group_names[0]}' – {years[0]}<br><sub>procenty</sub>",
        "Miesiąc",
        "Sport"
    )


def make_plot_2(df):
    top_body_parts = df['Body_Part'].value_counts().nlargest(10).index.tolist()
    years = sorted(df['Year'].unique())

    pivot = (
        df[df['Body_Part'].isin(top_body_parts)]
        .groupby(['Year', 'Body_Part'])
        .size()
        .reset_index(name='count')
        .pivot(index='Year', columns='Body_Part', values='count')
        .reindex(index=years, columns=top_body_parts, fill_value=0)
    )
    totals = df.groupby('Year').size().reindex(years)
    pct = pivot.div(totals, axis=0) * 100

    traces = []
    for i, yr in enumerate(years):
        color = COLUMN_COLORS_BAR[i % len(COLUMN_COLORS_BAR)]
        traces.append(go.Bar(
            x=top_body_parts,
            y=pct.loc[yr].values,
            name=str(yr),
            marker_color=color,
            hovertemplate='<b>%{x}</b><br>Rok %{fullData.name}: <b>%{y:.1f}%</b><extra></extra>'
        ))

    fig = go.Figure(data=traces)

    fig.update_layout(
        barmode='group',
        yaxis=dict(range=[0, 25]),
        margin=dict(l=80, t=100, r=150, b=120),
        xaxis=dict(tickangle=45)
    )

    return apply_theme(
        fig,
        "Trendy urazowości wg części ciała (procenty) w latach",
        "Część ciała",
        "Procent urazów (%)",
        legend_side=True
    )


def make_plot_3(df, sport_groups):
    flows = (
        df.groupby(['Race','Sport'])
        .size()
        .reset_index(name='count')
    )
    flows['value'] = flows['count'] / flows.groupby('Race')['count'].transform('sum')

    race_groups = {
        "Wszystkie rasy": sorted(df['Race'].dropna().unique()),
        "Top 3 najczęstsze": df['Race'].value_counts().nlargest(3).index.tolist()
    }
    sankey = {}
    for rg_name, rg in race_groups.items():
        sankey[rg_name] = {}
        for sg_name, sg in sport_groups.items():
            sub = flows[flows.Race.isin(rg) & flows.Sport.isin(sg)]
            nodes = rg + sg
            idx = {n:i for i,n in enumerate(nodes)}
            sankey[rg_name][sg_name] = dict(
                labels=nodes,
                source=sub.Race.map(idx).tolist(),
                target=sub.Sport.map(idx).tolist(),
                value =sub.value.tolist()
            )

    node_color = "#6272A4"   # łagodny fioletowo-szary odcień
    link_color = "rgba(98, 114, 164, 0.4)"  # półprzezroczysty

    init_rg, init_sg = next(iter(race_groups)), next(iter(sport_groups))
    data0 = sankey[init_rg][init_sg]

    fig = go.Figure(go.Sankey(
        node=dict(
            label=data0['labels'],
            pad=15,
            thickness=20,
            color=node_color
        ),
        link=dict(
            source=data0['source'],
            target=data0['target'],
            value=data0['value'],
            color=link_color,
            hovertemplate='<b>%{source.label}</b> → <b>%{target.label}</b><br>Udział: <b>%{value:.1%}</b><extra></extra>'
        )
    ))

    buttons = []
    for rg_name in race_groups:
        for sg_name in sport_groups:
            d = sankey[rg_name][sg_name]
            buttons.append(dict(
                label=f"{rg_name} → {sg_name}",
                method="update",
                args=[{
                    "node.label":  [d['labels']],
                    "link.source":[d['source']],
                    "link.target":[d['target']],
                    "link.value": [d['value']],
                    # kolory pozostają niezmienione
                },{"title":f"Przepływ urazów: {rg_name} → {sg_name}<br><sub>znormalizowane</sub>"}]
            ))

    fig.update_layout(
        updatemenus=[dict(
            buttons=buttons,
            direction="down",
            showactive=True,
            x=1, y=1.27,
            xanchor="right", yanchor="top",
            pad={"r":10, "t":10},
            font=dict(size=12, color=COLOR_PALETTE['text_secondary']),
            bgcolor='rgba(40, 42, 54, 0.8)',
            bordercolor=COLOR_PALETTE['neutral_light'],
            borderwidth=1
        )],
        margin=dict(t=80, r=50)
    )
    
    return apply_theme(
        fig,
        f"Przepływ urazów: {init_rg} → {init_sg}<br><sub>znormalizowane</sub>"
    )


def make_plot_4(df, sports_groups, race, body_group_key, sport_group_key):
    BODY_GROUPS = {
        "Top 10 części ciała": df['Body_Part'].value_counts().nlargest(10).index.tolist(),
        "Ręce":               ["SHOULDER", "UPPER ARM", "HAND", "LOWER ARM", "FINGER", "WRIST", "ELBOW"],
        "Nogi":               ["TOE", "ANKLE", "KNEE", "FOOT", "LOWER LEG", "UPPER LEG"],
        "Głowa":              ["FACE", "EAR", "HEAD", "NECK", "EYEBALL", "MOUTH"]
    }
    bg_list = BODY_GROUPS[body_group_key]
    sg_list = sports_groups[sport_group_key]
    cmin, cmax = 0, 35

    sub_all = df[(df['Race'] == race) & (df['Sport'].isin(sg_list))]
    denom = sub_all.groupby('Sport').size().reindex(sg_list, fill_value=0)

    sub = sub_all[sub_all['Body_Part'].isin(bg_list)]
    pivot = (
        sub.groupby(['Body_Part', 'Sport'])
           .size()
           .reset_index(name='count')
           .pivot(index='Body_Part', columns='Sport', values='count')
           .reindex(index=bg_list, columns=sg_list, fill_value=0)
    )

    pivot_pct = pivot.div(denom.replace(0, 1), axis=1) * 100

    fig = go.Figure(go.Heatmap(
        z=pivot_pct.values,
        x=pivot_pct.columns.tolist(),
        y=pivot_pct.index.tolist(),
        colorscale=colorscale_heatmap,
        coloraxis="coloraxis",
        hovertemplate='<b>%{y}</b> w <b>%{x}</b><br><b>%{z:.1f}%</b> urazów<extra></extra>'
    ))

    fig.update_layout(
        coloraxis=dict(
            colorscale=colorscale_heatmap,
            cmin=cmin,
            cmax=cmax,
            colorbar=dict(
                title="Procent urazów",
                titlefont=dict(size=14, color=COLOR_PALETTE['text_primary']),
                tickfont=dict(size=12, color=COLOR_PALETTE['text_secondary'])
            )
        ),
        margin=dict(t=120, r=100, b=80, l=150),
        xaxis=dict(tickangle=45)
    )

    return apply_theme(
        fig,
        f"Rozkład ilości urazów względem części ciała i dyscypiny: {race} – {body_group_key} vs {sport_group_key}<br><sub>procenty</sub>",
        "Sport",
        "Część ciała"
    )


def make_plot_5(df):
    top_sports = df['Sport'].value_counts().nlargest(10).index.tolist()
    sexes = ['MALE', 'FEMALE']

    sport_counts = {}
    for sex in sexes:
        counts = (
            df.loc[df['Sex'] == sex, 'Sport']
            .value_counts()
            .reindex(top_sports, fill_value=0)
        )
        sport_counts[sex] = counts.values

    # Create traces with the larger value first (so smaller is drawn on top)
    traces = []
    for i, sport in enumerate(top_sports):
        male_count = sport_counts['MALE'][i]
        female_count = sport_counts['FEMALE'][i]
        
        # Draw the larger bar first, smaller bar second
        if male_count >= female_count:
            first_trace = go.Bar(
                x=[male_count],
                y=[sport],
                name='Mężczyźni' if i == 0 else '',  # Only show legend once
                orientation='h',
                marker_color=COLOR_PALETTE['pl1'],
                hovertemplate='<b>%{y}</b><br>Mężczyźni: <b>%{x}</b><extra></extra>',
                showlegend=(i == 0)
            )
            second_trace = go.Bar(
                x=[female_count],
                y=[sport],
                name='Kobiety' if i == 0 else '',
                orientation='h',
                marker_color=COLOR_PALETTE['pl2'],
                hovertemplate='<b>%{y}</b><br>Kobiety: <b>%{x}</b><extra></extra>',
                showlegend=(i == 0)
            )
        else:
            first_trace = go.Bar(
                x=[female_count],
                y=[sport],
                name='Kobiety' if i == 0 else '',
                orientation='h',
                marker_color=COLOR_PALETTE['pl2'],
                hovertemplate='<b>%{y}</b><br>Kobiety: <b>%{x}</b><extra></extra>',
                showlegend=(i == 0)
            )
            second_trace = go.Bar(
                x=[male_count],
                y=[sport],
                name='Mężczyźni' if i == 0 else '',
                orientation='h',
                marker_color=COLOR_PALETTE['pl1'],
                hovertemplate='<b>%{y}</b><br>Mężczyźni: <b>%{x}</b><extra></extra>',
                showlegend=(i == 0)
            )
        
        traces.extend([first_trace, second_trace])

    fig = go.Figure(data=traces)
    fig.update_layout(
        barmode='overlay',
        xaxis=dict(
            range=[0, max(sport_counts['MALE'].max(), sport_counts['FEMALE'].max()) * 1.1]
        ),
        margin=dict(l=200, t=100, r=150, b=80)
    )

    return apply_theme(
        fig,
        "Różnice płci w urazowości (top 10 sportów)",
        "Liczba urazów",
        "Sport"
    )

def make_plot_6(df):
    top_sports = df['Sport'].value_counts().nlargest(6).index.tolist()
    ages = sorted(df.loc[(df['Age'] >= 15) & (df['Age'] <= 100), 'Age'].unique())
    years = sorted(df['Year'].unique())

    age_sport_year = {}
    for age in ages:
        sub = df[df['Age'] == age]
        cnt = sub.groupby(['Sport', 'Year']).size().unstack(fill_value=0)
        cnt = cnt.reindex(index=top_sports, columns=years, fill_value=0)
        age_sport_year[age] = {sport: cnt.loc[sport].values for sport in top_sports}

    traces = []
    for i, age in enumerate(ages):
        for j, sport in enumerate(top_sports):
            counts = age_sport_year[age][sport]
            color = COLUMN_COLORS_BAR[j % len(COLUMN_COLORS_BAR)]
            traces.append(go.Box(
                x=counts,
                y=[sport] * len(counts),
                name=sport,
                orientation='h',
                width=0.6,
                visible=(i == 0),
                marker_color=color,
                line=dict(color=color),
                hovertemplate='<b>%{y}</b><br>Mediana: %{x}<extra></extra>'
            ))

    fig = go.Figure(data=traces)
    steps = []
    n = len(top_sports)
    for i, age in enumerate(ages):
        vis = [False] * len(traces)
        for j in range(n):
            vis[i * n + j] = True
        steps.append({
            "method": "update",
            "label": str(age),
            "args": [
                {"visible": vis},
                {"title": f"Rozkład urazów dla wieku {age} lat<br><sub>boxploty za lata {min(years)}–{max(years)}</sub>"}
            ]
        })

    sliders = [dict(
        active=0,
        currentvalue={"prefix": "Wiek: "},
        pad={"t": 50},
        steps=steps,
        font=FONT_CONFIG['axis_labels']
    )]

    fig.update_layout(
        sliders=sliders,
        margin=dict(l=150, t=120, r=80, b=80),
        boxmode='group',
        showlegend=False,
        height=600
    )

    return apply_theme(
        fig,
        f"Rozkład urazów: wiek {ages[0]} lat<br><sub>lata {min(years)}–{max(years)}</sub>",
        "Liczba urazów rocznie",
        "Sport",
        height=600
    )


def make_plot_7(df):
    years = sorted(df['Year'].unique())
    sexes = ['MALE', 'FEMALE']
    colors = {'MALE': COLOR_PALETTE['pl1'], 'FEMALE': COLOR_PALETTE['pl2']}

    modes = [
        ('Wszystkie urazy', None),
        ('Pod wpływem alkoholu', 'Alcohol'),
        ('Pod wpływem narkotyków', 'Drug')
    ]

    traces = []
    for mode_name, col in modes:
        for sex in sexes:
            if col is None:
                cnt = (
                    df[df['Sex'] == sex]
                    .groupby('Year')
                    .size()
                    .reindex(years, fill_value=0)
                )
            else:
                cnt = (
                    df[(df['Sex'] == sex) & (df[col] == 1)]
                    .groupby('Year')
                    .size()
                    .reindex(years, fill_value=0)
                )
            display_name = 'Mężczyźni' if sex == 'MALE' else 'Kobiety'
            traces.append(go.Scatter(
                x=years,
                y=cnt.values,
                mode='lines+markers',
                name=display_name,
                legendgroup=sex,
                visible=(mode_name == 'Wszystkie urazy'),
                line=dict(width=3, color=colors[sex]),
                marker=dict(size=8, color=colors[sex]),
                hovertemplate='<b>%{fullData.name}</b><br>Rok %{x}: <b>%{y}</b> urazów<extra></extra>'
            ))

    fig = go.Figure(data=traces)

    buttons = []
    for i, (mode_name, _) in enumerate(modes):
        vis = [False] * (len(modes) * len(sexes))
        start = i * len(sexes)
        for j in range(len(sexes)):
            vis[start + j] = True
        buttons.append({
            "method": "update",
            "label": mode_name,
            "args": [
                {"visible": vis},
                {"title": f"Trendy urazów: {mode_name} (lata {min(years)}–{max(years)})"}
            ]
        })

    updatemenus = [dict(
        type="dropdown",
        buttons=buttons,
        x=1.15, y=1,
        xanchor="left", yanchor="top",
        font=FONT_CONFIG['axis_labels'],
        bgcolor='rgba(255, 255, 255, 0.9)',
        bordercolor=COLOR_PALETTE['neutral_light'],
        borderwidth=1
    )]

    fig.update_layout(
        updatemenus=updatemenus,
        xaxis=dict(
            tickmode="linear",
            dtick=1
        ),
        legend=dict(
            title="Płeć",
            itemclick="toggle",
            itemdoubleclick="toggleothers"
        ),
        margin=dict(l=80, t=120, r=200, b=80)
    )

    return apply_theme(
        fig,
        f"Trendy urazów: Wszystkie urazy (lata {min(years)}–{max(years)})",
        "Rok",
        "Liczba urazów"
    )

def make_plot_8(df):
    top_sports = df['Sport'].value_counts().nlargest(10).index.tolist()
    total_counts_by_sport = df['Sport'].value_counts().reindex(top_sports, fill_value=0)

    modes = [
        ('Pod wpływem alkoholu', 'Alcohol'),
        ('Pod wpływem narkotyków', 'Drug'),
        ('Alkohol i narkotyki jednocześnie', 'Both'),
    ]

    category_colors = {
        'Pod wpływem alkoholu': COLOR_PALETTE['accent1'],
        'Pod wpływem narkotyków': COLOR_PALETTE['accent3'],
        'Alkohol i narkotyki jednocześnie': COLOR_PALETTE['accent5']
    }

    traces = []
    for mode_name, col in modes:
        if col == 'Both':
            sub = df[(df['Alcohol'] == 1) & (df['Drug'] == 1)]
        else:
            sub = df[df[col] == 1]

        category_counts = (
            sub['Sport']
            .value_counts()
            .reindex(top_sports, fill_value=0)
        )

        percentages = []
        for sport in top_sports:
            total_for_sport = total_counts_by_sport[sport]
            category_for_sport = category_counts[sport]
            pct = (category_for_sport / total_for_sport) * 100 if total_for_sport > 0 else 0.0
            percentages.append(pct)

        traces.append(go.Bar(
            x=top_sports,
            y=percentages,
            name=mode_name,
            marker_color=category_colors[mode_name],
            hovertemplate='<b>%{x}</b><br>%{fullData.name}: <b>%{y:.1f}%</b><extra></extra>'
        ))

    fig = go.Figure(data=traces)
    fig.update_layout(
        height=600,
        barmode='group',
        margin=dict(l=80, t=120, r=150, b=140),
        xaxis=dict(tickangle=45)
    )

    return apply_theme(
        fig,
        'Wpływ substancji psychoaktywnych na urazowość sportową<br><sub>Procent urazów w top 10 sportach</sub>',
        'Sport',
        'Procent urazów (%)',
        height=600,
        legend_side=True
    )