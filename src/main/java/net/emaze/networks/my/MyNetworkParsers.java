package net.emaze.networks.my;

import net.emaze.dysfunctional.contracts.dbc;

public class MyNetworkParsers {

    public static MyNetwork parse(String cidr) {
        dbc.precondition(cidr != null, "cidr must be not-null");
        dbc.precondition(cidr.contains("/") && cidr.indexOf("/") == cidr.lastIndexOf("/"), "cidr must contain / separator only once");
        final String[] split = cidr.split("/");
        final MyIp ip = MyIpParsers.parse(split[0]);
        final MyMask mask = ip.version().mask(Integer.parseInt(split[1]));
        return new MyNetwork(ip, mask);
    }
}
