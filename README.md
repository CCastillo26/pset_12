# PSET 12

The simulation study will begin by reading in the bike-share data. As in last week's lab, we will extract hour from the times using hour(start_time) and hour(end_time) before aggregating trips by (start_station, end_station, hour). Then, we will estimate hourly rates of arrival, or $\lambda(t)$, with the function estimate_arrival_rates() from the same assignment. Since actual $\lambda(t)$ changes by hour, we will simulate from the maximum arrival rate, or $\lambda_{max}$, across time. Then, we will thin those arrivals by accepting each probability with $p_i = \lambda(t_i)/\lambda_{max}$.

We will then simulate bike use throughout a day. If a bike is available at the start_station, the trip will be considered successful, and the number of available bikes there will decrease (bikes - 1). Conversely, if there are no bikes at the start_station, the trip will be considered unsuccessful (bikes + 1).

We will then simulate proposed arrival times $e_j \sim \text{Exp}(\lambda_{max})$ and construct arrival times $t_i = \sum_{j = 1}^{i} e_j$ by summing each draw. However, since these times will be oversimulated, as they depend on $\lambda_{max}$, we will thin them. For each proposed arrival time $t_i$, we will compute the acceptance probability $p_i = \lambda(t_i)/\lambda_{max}$. Then, we will draw $u_i \sim \text{Unif}(0, 1)$ and keep the observation if $u_i \leq p_i$ and remove otherwise.

Lastly, these values will be placed in a two-column data frame with one column for the station's associated number and the other for the recommended number of bikes to place there at the start of the day.
