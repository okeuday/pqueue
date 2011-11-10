%% Skew heap solution based on Ulf Wiger's suggestion

-module(pqueue2).
-export([new/0, is_empty/1, in/3, out/1, pout/1, test/0]).

new() ->
    empty.

is_empty(empty) ->
    true;
is_empty(_) ->
    false.

in(Value, P, H) ->
    merge({P, empty, empty, element, Value}, H).

out(empty) ->
    {empty, empty};
out({P, HL, HR, queue, Queue}) ->
    case queue:out(Queue) of
        {{value, Value}, NewQueue} ->
            {{value, Value}, {P, HL, HR, queue, NewQueue}};
        {empty, _} ->
            out(merge(HL, HR))
    end;
out({_, HL, HR, element, Value}) ->
    {{value, Value}, merge(HL, HR)}.

pout(empty) ->
    {empty, empty};
pout({P, HL, HR, queue, Queue}) ->
    case queue:out(Queue) of
        {{value, Value}, NewQueue} ->
            {{value, Value, P}, {P, HL, HR, queue, NewQueue}};
        {empty, _} ->
            pout(merge(HL, HR))
    end;
pout({P, HL, HR, element, Value}) ->
    {{value, Value, P}, merge(HL, HR)}.

merge(empty, H) ->
    H;
merge(H, empty) ->
    H;
merge({P1, HL1, HR1, T, D}, {P2, _, _, _, _} = H2) when P1 < P2 ->
    {P1, HL1, merge(HR1, H2), T, D};
merge({P1, _, _, _, _} = H1, {P2, HL2, HR2, T, D}) when P1 > P2 ->
    {P2, merge(H1, HL2), HR2, T, D};
merge({P, HL1, HR1, element, Value1}, {P, HL2, HR2, element, Value2}) ->
    {P, merge(HL1, HR1), merge(HL2, HR2), queue,
     queue:in(Value2, queue:in(Value1, queue:new()))};
merge({P, HL1, HR1, queue, Queue}, {P, HL2, HR2, element, Value}) ->
    {P, merge(HL1, HR1), merge(HL2, HR2), queue, queue:in(Value, Queue)};
merge({P, HL1, HR1, element, Value}, {P, HL2, HR2, queue, Queue}) ->
    {P, merge(HL1, HR1), merge(HL2, HR2), queue, queue:in(Value, Queue)}.

test() ->
    Q0 = pqueue2:new(),
    Q1 = pqueue2:in(20, 20, Q0),
    Q2 = pqueue2:in(19, 19, Q1),
    Q3 = pqueue2:in(18, 18, Q2),
    Q4 = pqueue2:in(17, 17, Q3),
    Q5 = pqueue2:in(16, 16, Q4),
    Q6 = pqueue2:in(15, 15, Q5),
    Q7 = pqueue2:in(14, 14, Q6),
    Q8 = pqueue2:in(13, 13, Q7),
    Q9 = pqueue2:in(12, 12, Q8),
    Q10 = pqueue2:in(11, 11, Q9),
    Q11 = pqueue2:in(10, 10, Q10),
    Q12 = pqueue2:in(9, 9, Q11),
    Q13 = pqueue2:in(8, 8, Q12),
    Q14 = pqueue2:in(7, 7, Q13),
    Q15 = pqueue2:in(6, 6, Q14),
    Q16 = pqueue2:in(5, 5, Q15),
    Q17 = pqueue2:in(4, 4, Q16),
    Q18 = pqueue2:in(3, 3, Q17),
    Q19 = pqueue2:in(2, 2, Q18),
    Q20 = pqueue2:in(1, 1, Q19),
    Q21 = pqueue2:in(0, 0, Q20),
    Q22 = pqueue2:in(-1, -1, Q21),
    Q23 = pqueue2:in(-2, -2, Q22),
    Q24 = pqueue2:in(-3, -3, Q23),
    Q25 = pqueue2:in(-4, -4, Q24),
    Q26 = pqueue2:in(-5, -5, Q25),
    Q27 = pqueue2:in(-6, -6, Q26),
    Q28 = pqueue2:in(-7, -7, Q27),
    Q29 = pqueue2:in(-8, -8, Q28),
    Q30 = pqueue2:in(-9, -9, Q29),
    Q31 = pqueue2:in(-10, -10, Q30),
    Q32 = pqueue2:in(-11, -11, Q31),
    Q33 = pqueue2:in(-12, -12, Q32),
    Q34 = pqueue2:in(-13, -13, Q33),
    Q35 = pqueue2:in(-14, -14, Q34),
    Q36 = pqueue2:in(-15, -15, Q35),
    Q37 = pqueue2:in(-16, -16, Q36),
    Q38 = pqueue2:in(-17, -17, Q37),
    Q39 = pqueue2:in(-18, -18, Q38),
    Q40 = pqueue2:in(-19, -19, Q39),
    Q41 = pqueue2:in(-20, -20, Q40),
    Q42 = pqueue2:in(-20, -20, Q41),
    Q43 = pqueue2:in(-19, -19, Q42),
    Q44 = pqueue2:in(-18, -18, Q43),
    Q45 = pqueue2:in(-17, -17, Q44),
    Q46 = pqueue2:in(-16, -16, Q45),
    Q47 = pqueue2:in(-15, -15, Q46),
    Q48 = pqueue2:in(-14, -14, Q47),
    Q49 = pqueue2:in(-13, -13, Q48),
    Q50 = pqueue2:in(-12, -12, Q49),
    Q51 = pqueue2:in(-11, -11, Q50),
    Q52 = pqueue2:in(-10, -10, Q51),
    Q53 = pqueue2:in(-9, -9, Q52),
    Q54 = pqueue2:in(-8, -8, Q53),
    Q55 = pqueue2:in(-7, -7, Q54),
    Q56 = pqueue2:in(-6, -6, Q55),
    Q57 = pqueue2:in(-5, -5, Q56),
    Q58 = pqueue2:in(-4, -4, Q57),
    Q59 = pqueue2:in(-3, -3, Q58),
    Q60 = pqueue2:in(-2, -2, Q59),
    Q61 = pqueue2:in(-1, -1, Q60),
    Q62 = pqueue2:in(0, 0, Q61),
    Q63 = pqueue2:in(1, 1, Q62),
    Q64 = pqueue2:in(2, 2, Q63),
    Q65 = pqueue2:in(3, 3, Q64),
    Q66 = pqueue2:in(4, 4, Q65),
    Q67 = pqueue2:in(5, 5, Q66),
    Q68 = pqueue2:in(6, 6, Q67),
    Q69 = pqueue2:in(7, 7, Q68),
    Q70 = pqueue2:in(8, 8, Q69),
    Q71 = pqueue2:in(9, 9, Q70),
    Q72 = pqueue2:in(10, 10, Q71),
    Q73 = pqueue2:in(11, 11, Q72),
    Q74 = pqueue2:in(12, 12, Q73),
    Q75 = pqueue2:in(13, 13, Q74),
    Q76 = pqueue2:in(14, 14, Q75),
    Q77 = pqueue2:in(15, 15, Q76),
    Q78 = pqueue2:in(16, 16, Q77),
    Q79 = pqueue2:in(17, 17, Q78),
    Q80 = pqueue2:in(18, 18, Q79),
    Q81 = pqueue2:in(19, 19, Q80),
    Q82 = pqueue2:in(20, 20, Q81),
    {{value, -20}, Q83} = pqueue2:out(Q82),
    {{value, -20}, Q84} = pqueue2:out(Q83),
    {{value, -19}, Q85} = pqueue2:out(Q84),
    {{value, -19}, Q86} = pqueue2:out(Q85),
    {{value, -18}, Q87} = pqueue2:out(Q86),
    {{value, -18}, Q88} = pqueue2:out(Q87),
    %{{value, 0}, Q89} = pqueue2:out(0, Q88),
    %{{value, 0}, Q90} = pqueue2:out(0, Q89),
    %{empty, _} = pqueue2:out(0, Q90),
    {{value, -17, -17}, Q91} = pqueue2:pout(Q88),
    {{value, -17, -17}, Q92} = pqueue2:pout(Q91),
    {{value, -16, -16}, Q93} = pqueue2:pout(Q92),
    {{value, -16, -16}, Q94} = pqueue2:pout(Q93),
    {{value, -15, -15}, Q95} = pqueue2:pout(Q94),
    {{value, -15, -15}, Q96} = pqueue2:pout(Q95),
    {{value, -14, -14}, Q97} = pqueue2:pout(Q96),
    {{value, -14, -14}, Q98} = pqueue2:pout(Q97),
    {{value, -13, -13}, Q99} = pqueue2:pout(Q98),
    {{value, -13, -13}, Q100} = pqueue2:pout(Q99),
    {{value, -12, -12}, Q101} = pqueue2:pout(Q100),
    {{value, -12, -12}, Q102} = pqueue2:pout(Q101),
    {{value, -11, -11}, Q103} = pqueue2:pout(Q102),
    {{value, -11, -11}, Q104} = pqueue2:pout(Q103),
    {{value, -10, -10}, Q105} = pqueue2:pout(Q104),
    {{value, -10, -10}, Q106} = pqueue2:pout(Q105),
    {{value, -9, -9}, Q107} = pqueue2:pout(Q106),
    {{value, -9, -9}, Q108} = pqueue2:pout(Q107),
    {{value, -8, -8}, Q109} = pqueue2:pout(Q108),
    {{value, -8, -8}, Q110} = pqueue2:pout(Q109),
    {{value, -7, -7}, Q111} = pqueue2:pout(Q110),
    {{value, -7, -7}, Q112} = pqueue2:pout(Q111),
    {{value, -6, -6}, Q113} = pqueue2:pout(Q112),
    {{value, -6, -6}, Q114} = pqueue2:pout(Q113),
    {{value, -5, -5}, Q115} = pqueue2:pout(Q114),
    {{value, -5, -5}, Q116} = pqueue2:pout(Q115),
    {{value, -4, -4}, Q117} = pqueue2:pout(Q116),
    {{value, -4, -4}, Q118} = pqueue2:pout(Q117),
    {{value, -3, -3}, Q119} = pqueue2:pout(Q118),
    {{value, -3, -3}, Q120} = pqueue2:pout(Q119),
    {{value, -2, -2}, Q121} = pqueue2:pout(Q120),
    {{value, -2, -2}, Q122} = pqueue2:pout(Q121),
    {{value, -1, -1}, Q123} = pqueue2:pout(Q122),
    {{value, -1, -1}, Q123a} = pqueue2:pout(Q123),
    {{value, 0, 0}, Q123b} = pqueue2:pout(Q123a),
    {{value, 0, 0}, Q124} = pqueue2:pout(Q123b),
    {{value, 1, 1}, Q125} = pqueue2:pout(Q124),
    {{value, 1, 1}, Q126} = pqueue2:pout(Q125),
    {{value, 2, 2}, Q127} = pqueue2:pout(Q126),
    {{value, 2, 2}, Q128} = pqueue2:pout(Q127),
    {{value, 3, 3}, Q129} = pqueue2:pout(Q128),
    {{value, 3, 3}, Q130} = pqueue2:pout(Q129),
    {{value, 4, 4}, Q131} = pqueue2:pout(Q130),
    {{value, 4, 4}, Q132} = pqueue2:pout(Q131),
    {{value, 5, 5}, Q133} = pqueue2:pout(Q132),
    {{value, 5, 5}, Q134} = pqueue2:pout(Q133),
    {{value, 6, 6}, Q135} = pqueue2:pout(Q134),
    {{value, 6, 6}, Q136} = pqueue2:pout(Q135),
    {{value, 7, 7}, Q137} = pqueue2:pout(Q136),
    {{value, 7, 7}, Q138} = pqueue2:pout(Q137),
    {{value, 8, 8}, Q139} = pqueue2:pout(Q138),
    {{value, 8, 8}, Q140} = pqueue2:pout(Q139),
    {{value, 9, 9}, Q141} = pqueue2:pout(Q140),
    {{value, 9, 9}, Q142} = pqueue2:pout(Q141),
    {{value, 10, 10}, Q143} = pqueue2:pout(Q142),
    {{value, 10, 10}, Q144} = pqueue2:pout(Q143),
    {{value, 11, 11}, Q145} = pqueue2:pout(Q144),
    {{value, 11, 11}, Q146} = pqueue2:pout(Q145),
    {{value, 12, 12}, Q147} = pqueue2:pout(Q146),
    {{value, 12, 12}, Q148} = pqueue2:pout(Q147),
    {{value, 13, 13}, Q149} = pqueue2:pout(Q148),
    {{value, 13, 13}, Q150} = pqueue2:pout(Q149),
    {{value, 14, 14}, Q151} = pqueue2:pout(Q150),
    {{value, 14, 14}, Q152} = pqueue2:pout(Q151),
    {{value, 15, 15}, Q153} = pqueue2:pout(Q152),
    {{value, 15, 15}, Q154} = pqueue2:pout(Q153),
    {{value, 16, 16}, Q155} = pqueue2:pout(Q154),
    {{value, 16, 16}, Q156} = pqueue2:pout(Q155),
    {{value, 17, 17}, Q157} = pqueue2:pout(Q156),
    {{value, 17, 17}, Q158} = pqueue2:pout(Q157),
    {{value, 18, 18}, Q159} = pqueue2:pout(Q158),
    {{value, 18, 18}, Q160} = pqueue2:pout(Q159),
    {{value, 19, 19}, Q161} = pqueue2:pout(Q160),
    {{value, 19, 19}, Q162} = pqueue2:pout(Q161),
    {{value, 20, 20}, Q163} = pqueue2:pout(Q162),
    {{value, 20, 20}, Q164} = pqueue2:pout(Q163),
    false = pqueue2:is_empty(Q164),
    {empty, Q165} = pqueue2:pout(Q164),
    true = pqueue2:is_empty(Q165),
    ok.
