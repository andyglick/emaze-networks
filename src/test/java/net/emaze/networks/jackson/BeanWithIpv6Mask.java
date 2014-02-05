package net.emaze.networks.jackson;

import net.emaze.networks.Ipv6Mask;

public class BeanWithIpv6Mask {

    private Ipv6Mask netmask;

    public Ipv6Mask getNetmask() {
        return netmask;
    }

    public void setNetmask(Ipv6Mask netmask) {
        this.netmask = netmask;
    }

}
