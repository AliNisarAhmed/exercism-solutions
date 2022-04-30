use std::cmp::Ordering;
use std::collections::HashMap;

pub fn tally(match_results_str: &str) -> String {
    let header = tourney_table_header();
    if match_results_str.is_empty() {
        return header;
    }

    let tally = tally_matches(parse_matches(match_results_str));

    let mut sorted_values: Vec<&Stats> = tally.values().collect();

    sorted_values.sort_by(|s1, s2| match s2.points.cmp(&s1.points) {
        Ordering::Equal => s1.team_name.cmp(&s2.team_name),
        x => x,
    });

    header
        + "\n"
        + &sorted_values
            .into_iter()
            .map(tourney_table_body)
            .collect::<Vec<String>>()
            .join("\n")
}

struct Match {
    homeTeam: String,
    awayTeam: String,
    result: MatchResult,
}

#[derive(Copy, Clone)]
enum MatchResult {
    Win,
    Lose,
    Draw,
}

impl MatchResult {
    fn away_team_result(self) -> Self {
        match self {
            MatchResult::Win => MatchResult::Lose,
            MatchResult::Lose => MatchResult::Win,
            x => x,
        }
    }
}

impl std::str::FromStr for MatchResult {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "win" => Ok(MatchResult::Win),
            "loss" => Ok(MatchResult::Lose),
            "draw" => Ok(MatchResult::Draw),
            _ => Err(format!("'{}' is not a valid value for MatchResult", s)),
        }
    }
}

#[derive(Debug)]
struct Stats {
    team_name: String,
    matches_played: u32,
    wins: u32,
    draws: u32,
    losses: u32,
    points: u32,
}

impl Stats {
    fn new(team_name: String) -> Self {
        Stats {
            team_name,
            matches_played: 0,
            wins: 0,
            losses: 0,
            draws: 0,
            points: 0,
        }
    }

    fn modify(&mut self, m: MatchResult) -> &mut Self {
        match m {
            MatchResult::Win => {
                self.matches_played += 1;
                self.wins += 1;
                self.points += 3;
            }
            MatchResult::Lose => {
                self.matches_played += 1;
                self.losses += 1;
            }
            MatchResult::Draw => {
                self.matches_played += 1;
                self.draws += 1;
                self.points += 1
            }
        }
        self
    }
}

fn parse_match(line: &str) -> Match {
    let vec: Vec<&str> = line.split(";").collect();

    Match {
        homeTeam: vec[0].to_owned(),
        awayTeam: vec[1].to_owned(),
        result: vec[2].parse().unwrap(),
    }
}

fn parse_matches(input: &str) -> Vec<Match> {
    input.split("\n").map(parse_match).collect()
}

fn tally_match<'a>(
    store: &'a mut HashMap<String, Stats>,
    m: Match,
) -> &'a mut HashMap<String, Stats> {
    store
        .entry(m.homeTeam.clone())
        .or_insert(Stats::new(m.homeTeam))
        .modify(m.result);

    store
        .entry(m.awayTeam.clone())
        .or_insert(Stats::new(m.awayTeam))
        .modify(m.result.away_team_result());

    store
}

fn tally_matches(ms: Vec<Match>) -> HashMap<String, Stats> {
    let mut store = HashMap::new();
    ms.into_iter().fold(&mut store, tally_match);

    store
}

fn tourney_table_header() -> String {
    String::from(format!("{0:31}| MP |  W |  D |  L |  P", "Team"))
}

fn tourney_table_body(s: &Stats) -> String {
    String::from(format!(
        "{0:31}|{1:3} |{2:3} |{3:3} |{4:3} |{5:3}",
        s.team_name, s.matches_played, s.wins, s.draws, s.losses, s.points
    ))
}
