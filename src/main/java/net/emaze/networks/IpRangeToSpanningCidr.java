package net.emaze.networks;

import net.emaze.dysfunctional.contracts.dbc;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.Order;

public class IpRangeToSpanningCidr implements BinaryDelegate<Cidr, Ipv4, Ipv4> {

    @Override
    public Cidr perform(Ipv4 firstIp, Ipv4 lastIp) {
        dbc.precondition(firstIp != null, "startIp cannot be null");
        dbc.precondition(lastIp != null, "endIp cannot be null");
        dbc.precondition(Order.of(firstIp.compareTo(lastIp)) != Order.GT, "endIp cannot be lesser than startIp");
        Netmask netmask = null;
        Cidr candidate;
        do {
            netmask = (netmask == null ? Netmask.fromBits(32) : netmask.widen());
            candidate = new Cidr(lastIp, netmask);
        } while (!candidate.contains(firstIp));
        return candidate;
    }
}
