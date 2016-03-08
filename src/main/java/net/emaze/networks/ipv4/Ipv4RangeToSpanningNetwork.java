package net.emaze.networks.ipv4;

import java.util.function.BiFunction;
import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.order.Order;

public class Ipv4RangeToSpanningNetwork implements BiFunction<Ipv4, Ipv4, Ipv4Network> {

    @Override
    public Ipv4Network apply(Ipv4 firstIp, Ipv4 lastIp) {
        dbc.precondition(firstIp != null, "firstIp cannot be null");
        dbc.precondition(lastIp != null, "lastIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "lastIp cannot be lesser than firstIp");
        Ipv4Mask netmask = null;
        Ipv4Network candidate;
        do {
            netmask = (netmask == null ? Ipv4Mask.getNarrowestMask() : netmask.widenHosts());
            candidate = Ipv4Network.byContainedIp(lastIp, netmask);
        } while (!candidate.contains(firstIp));
        return candidate;
    }
}
