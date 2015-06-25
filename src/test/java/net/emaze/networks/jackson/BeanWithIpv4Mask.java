package net.emaze.networks.jackson;

import net.emaze.networks.ipv4.Ipv4Mask;

public class BeanWithIpv4Mask {

    private Ipv4Mask netmask;

    public Ipv4Mask getNetmask() {
        return netmask;
    }

    public void setNetmask(Ipv4Mask netmask) {
        this.netmask = netmask;
    }

}
