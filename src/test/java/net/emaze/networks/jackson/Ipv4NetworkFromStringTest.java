package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import net.emaze.networks.Ipv4Network;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4NetworkFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'cidr':'10.0.0.0/8'}".replace("'", "\"");
        final Ipv4Network expected = Ipv4Network.fromCidrNotation("10.0.0.0/8");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv4Cidr got = mapper.readValue(serialized, BeanWithIpv4Cidr.class);
        Assert.assertEquals(expected, got.getCidr());
    }

}
