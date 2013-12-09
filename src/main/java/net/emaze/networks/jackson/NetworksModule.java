package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.module.SimpleModule;
import net.emaze.networks.Ipv4Network;
import net.emaze.networks.Ipv4;
import net.emaze.networks.Ipv6;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.Ipv6Network;
import net.emaze.networks.Ipv4Mask;

public class NetworksModule extends SimpleModule {

    public NetworksModule() {
        super("networks-module", new Version(2, 0, 0, null, "net.emaze", "networks"));
        this.addSerializer(Ipv4.class, new Ipv4ToString());
        this.addDeserializer(Ipv4.class, new Ipv4FromString());
        this.addSerializer(Ipv4Mask.class, new Ipv4MaskToString());
        this.addDeserializer(Ipv4Mask.class, new Ipv4MaskFromString());
        this.addSerializer(Ipv4Network.class, new Ipv4NetworkToString());
        this.addDeserializer(Ipv4Network.class, new Ipv4NetworkFromString());
        this.addSerializer(Ipv6.class, new Ipv6ToString());
        this.addDeserializer(Ipv6.class, new Ipv6FromString());
        this.addSerializer(Ipv6Mask.class, new Ipv6MaskToString());
        this.addDeserializer(Ipv6Mask.class, new Ipv6MaskFromString());
        this.addSerializer(Ipv6Network.class, new Ipv6NetworkToString());
        this.addDeserializer(Ipv6Network.class, new Ipv6NetworkFromString());
    }
}
