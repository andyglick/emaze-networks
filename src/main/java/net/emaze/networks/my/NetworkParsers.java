package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;

public class NetworkParsers {

    public static Network parse(String cidr) {
        dbc.precondition(cidr != null, "cidr must be not-null");
        dbc.precondition(cidr.contains("/") && cidr.indexOf("/") == cidr.lastIndexOf("/"), "cidr must contain / separator only once");
        final String[] split = cidr.split("/");
        final Ip ip = IpParsers.parse(split[0]);
        final Mask mask = ip.version().mask(Integer.parseInt(split[1]));
        return new Network(ip, mask);
    }
}
