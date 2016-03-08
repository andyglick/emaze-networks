package net.emaze.networks.ipv4;

import java.util.function.Function;
import net.emaze.dysfunctional.contracts.dbc;

public class Ipv4PreviousIp implements Function<Ipv4, Ipv4> {

    @Override
    public Ipv4 apply(Ipv4 ip) {
        dbc.precondition(ip != null, "ip cannot be null");
        return ip.previous();
    }
}
